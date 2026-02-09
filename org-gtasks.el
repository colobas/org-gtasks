;;; org-gtasks.el --- Sync org TODO items with Google Tasks -*- lexical-binding: t; -*-

;; Author: Julien Masson <massonju.eseo@gmail.com>
;; Maintainer: Guilherme Pires <gpires@altius.org>
;; URL: https://github.com/colobas/org-gtasks
;; Version: 0.2
;; Package-Requires: ((emacs "27.1") (cl-lib "0.5") (org "9.3") (request "0.3") (deferred "0.5"))
;; Keywords: convenience, calendar, tasks

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Heading-level bidirectional sync between existing Org files and Google Tasks.
;;
;; Unlike the original org-gtasks, this version does NOT create/own org files.
;; Instead it syncs individual TODO headings (matched by :GTASKS-ID: property)
;; within your existing org files.
;;
;; Two Google Tasks lists are used:
;;   - "org" list: Emacs pushes active TODOs here so you can see/complete them
;;     on your phone
;;   - "inbox" list: Tasks created on your phone get pulled into inbox.org
;;
;; Usage:
;;   (org-gtasks-register-account
;;     :name "work"
;;     :login "you@example.com"
;;     :client-id "..."
;;     :client-secret "..."
;;     :push-tasklist "org"
;;     :pull-tasklist "inbox"
;;     :inbox-file "~/org/inbox.org"
;;     :agenda-files-fn #'my-agenda-files-fn)
;;
;;   M-x org-gtasks-sync       ; bidirectional sync
;;   M-x org-gtasks-push       ; push org TODOs → Google Tasks
;;   M-x org-gtasks-pull       ; pull Google Tasks → org inbox

;;; Code:

(require 'cl-lib)
(require 'deferred)
(require 'json)
(require 'org)
(require 'org-element)
(require 'request-deferred)

;;;; ---- Data structures ----

(cl-defstruct org-gtasks-account
  (name nil :read-only t)
  login
  client-id
  client-secret
  access-token
  refresh-token
  ;; Config
  push-tasklist          ; name of the Google Tasks list to push TO (e.g. "org")
  pull-tasklist          ; name of the Google Tasks list to pull FROM (e.g. "inbox")
  inbox-file             ; path to inbox.org for pulled tasks
  agenda-files-fn        ; function returning list of org files to scan for push
  ;; Runtime state
  tasklist-ids)          ; alist of (name . id) for known tasklists

;;;; ---- Constants ----

(defconst org-gtasks-token-url "https://oauth2.googleapis.com/token")
(defconst org-gtasks-auth-url "https://accounts.google.com/o/oauth2/auth")
(defconst org-gtasks-resource-url "https://www.googleapis.com/auth/tasks")
(defconst org-gtasks-api-url "https://tasks.googleapis.com/tasks/v1")

(defconst org-gtasks--token-request-regexp
  "^[[:space:]]*GET[[:space:]]+[/?]+\\([[:graph:]]*\\)[[:space:]]+HTTP/[0-9.]+[[:space:]]*$")

;;;; ---- Customization ----

(defgroup org-gtasks nil
  "Sync org headings with Google Tasks."
  :group 'org
  :prefix "org-gtasks-")

(defcustom org-gtasks-gtasks-id-property "GTASKS_ID"
  "Org property name used to store the Google Tasks ID on a heading."
  :type 'string
  :group 'org-gtasks)

(defcustom org-gtasks-push-todo-states '("TODO" "WAIT")
  "List of TODO states that should be pushed to Google Tasks."
  :type '(repeat string)
  :group 'org-gtasks)

(defcustom org-gtasks-push-tag "personal"
  "Only push TODO items with this tag. If nil, push all eligible TODOs."
  :type '(choice (const :tag "Push all" nil)
                 (string :tag "Tag name"))
  :group 'org-gtasks)

(defcustom org-gtasks-token-directory user-emacs-directory
  "Directory where refresh tokens are stored."
  :type 'directory
  :group 'org-gtasks)

;;;; ---- Account registry ----

(defvar org-gtasks--accounts nil "List of registered `org-gtasks-account' structs.")
(defvar org-gtasks--token-tmp-vars nil "Temp vars for OAuth PKCE flow.")

;;;###autoload
(defun org-gtasks-register-account (&rest plist)
  "Register a Google Tasks account for syncing.

PLIST accepts:
  :name           Account name (string)
  :login          Google email
  :client-id      OAuth client ID
  :client-secret  OAuth client secret
  :push-tasklist  Name of Google Tasks list to push to (default \"org\")
  :pull-tasklist  Name of Google Tasks list to pull from (default \"inbox\")
  :inbox-file     Path to inbox org file for pulled tasks
  :agenda-files-fn  Function returning list of org files to scan"
  (let ((account (make-org-gtasks-account
                  :name (plist-get plist :name)
                  :login (plist-get plist :login)
                  :client-id (plist-get plist :client-id)
                  :client-secret (plist-get plist :client-secret)
                  :push-tasklist (or (plist-get plist :push-tasklist) "org")
                  :pull-tasklist (or (plist-get plist :pull-tasklist) "inbox")
                  :inbox-file (plist-get plist :inbox-file)
                  :agenda-files-fn (plist-get plist :agenda-files-fn))))
    (setq org-gtasks--accounts
          (cons account
                (cl-remove-if (lambda (a)
                                (string= (org-gtasks-account-name a)
                                         (org-gtasks-account-name account)))
                              org-gtasks--accounts)))))

(defun org-gtasks--find-account (name)
  "Find account by NAME."
  (cl-find-if (lambda (a) (string= (org-gtasks-account-name a) name))
              org-gtasks--accounts))

(defun org-gtasks--choose-account ()
  "Prompt user to choose an account if multiple, else return the only one."
  (if (= 1 (length org-gtasks--accounts))
      (car org-gtasks--accounts)
    (let* ((names (mapcar #'org-gtasks-account-name org-gtasks--accounts))
           (name (completing-read "Account: " names nil t)))
      (org-gtasks--find-account name))))

;;;; ---- Utilities ----

(defun org-gtasks--random-string (len)
  "Generate a random alphanumeric string of length LEN."
  (let ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXY0123456789-_"))
    (apply #'string (cl-loop repeat len
                             collect (aref chars (random (length chars)))))))

(defun org-gtasks--json-read ()
  "Parse JSON from current buffer."
  (let ((json-object-type 'plist))
    (goto-char (point-min))
    (when (re-search-forward "^{" nil t)
      (goto-char (1- (point))))
    (json-read-from-string
     (decode-coding-string
      (buffer-substring-no-properties (point) (point-max)) 'utf-8))))

(defun org-gtasks--format-org2iso (year mon day &optional hour min)
  "Convert org date components to ISO 8601 string."
  (let ((seconds (time-to-seconds
                  (encode-time 0 (or min 0) (or hour 0) day mon year))))
    (concat (format-time-string "%Y-%m-%dT%H:%M" (seconds-to-time seconds))
            ":00.000Z")))

(defun org-gtasks--format-iso2org (str)
  "Convert ISO 8601 STR to org timestamp string."
  (when (and str (not (string-empty-p str)))
    (format-time-string "%Y-%m-%d %a" (date-to-time str))))

;;;; ---- HTTP / Auth ----

(defun org-gtasks--token-file (account)
  "Return the refresh token file path for ACCOUNT."
  (expand-file-name (format ".org-gtasks-%s.token" (org-gtasks-account-name account))
                    org-gtasks-token-directory))

(defun org-gtasks--read-refresh-token (account)
  "Read saved refresh token for ACCOUNT."
  (let ((file (org-gtasks--token-file account)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (string-trim (buffer-string))))))

(defun org-gtasks--save-refresh-token (account)
  "Save refresh token for ACCOUNT to disk."
  (let ((file (org-gtasks--token-file account))
        (token (org-gtasks-account-refresh-token account)))
    (when token
      (with-temp-file file
        (insert token)))))

(defun org-gtasks--get-access-token (account)
  "Get a fresh access token for ACCOUNT using the refresh token."
  (let* ((resp (request (concat org-gtasks-token-url)
                :type "POST"
                :data `(("client_id" . ,(org-gtasks-account-client-id account))
                        ("client_secret" . ,(org-gtasks-account-client-secret account))
                        ("refresh_token" . ,(org-gtasks-account-refresh-token account))
                        ("grant_type" . "refresh_token"))
                :parser #'org-gtasks--json-read
                :sync t))
         (data (request-response-data resp)))
    (plist-get data :access_token)))

(defun org-gtasks--ensure-token (account)
  "Ensure ACCOUNT has valid access and refresh tokens."
  (unless (org-gtasks-account-refresh-token account)
    (setf (org-gtasks-account-refresh-token account)
          (org-gtasks--read-refresh-token account)))
  (unless (org-gtasks-account-refresh-token account)
    (org-gtasks--request-auth account)
    (error "Auth initiated — re-run after authorizing in browser"))
  (unless (org-gtasks-account-access-token account)
    (let ((token (org-gtasks--get-access-token account)))
      (if token
          (setf (org-gtasks-account-access-token account) token)
        (error "Failed to get access token for %s" (org-gtasks-account-name account))))))

(defun org-gtasks--request-auth (account)
  "Start OAuth2 PKCE flow for ACCOUNT."
  (setq org-gtasks--token-tmp-vars nil)
  (let* ((server-proc (org-gtasks--make-network-process account))
         (state (org-gtasks--random-string 8))
         (code-verifier (org-gtasks--random-string 43))
         (binary-hash (secure-hash 'sha256 code-verifier nil nil t))
         (code-challenge (base64url-encode-string binary-hash t))
         (local-url (format "http://localhost:%i/"
                            (cadr (process-contact server-proc))))
         (params `((scope . ,org-gtasks-resource-url)
                   (client_id . ,(org-gtasks-account-client-id account))
                   (redirect_uri . ,local-url)
                   (login_hint . ,(org-gtasks-account-login account))
                   (response_type . "code")
                   (response_mode . "query")
                   (access_type . "offline")
                   (state . ,state)
                   (code_challenge . ,code-challenge)
                   (code_challenge_method . "S256")))
         (query (mapconcat (lambda (p)
                             (concat (url-hexify-string (symbol-name (car p)))
                                     "=" (url-hexify-string (cdr p))))
                           params "&")))
    (setq org-gtasks--token-tmp-vars (list local-url code-verifier))
    (browse-url (concat org-gtasks-auth-url "?" query))))

(defun org-gtasks--make-network-process (account)
  "Create a local server to handle OAuth redirect for ACCOUNT."
  (make-network-process :name "org-gtasks-oauth2"
                        :service t
                        :server t
                        :host 'local
                        :family 'ipv4
                        :filter (apply-partially #'org-gtasks--oauth-filter account)
                        :coding 'binary))

(defun org-gtasks--oauth-filter (account process input)
  "Handle OAuth redirect for ACCOUNT from PROCESS with INPUT."
  (when-let* ((query (with-temp-buffer
                       (insert input)
                       (goto-char (point-min))
                       (when (re-search-forward org-gtasks--token-request-regexp nil t)
                         (mapcar (lambda (it) (cons (intern (car it)) (cadr it)))
                                 (url-parse-query-string (match-string 1))))))
              (code (assoc-default 'code query)))
    (process-send-string
     process (concat "HTTP/1.0 200 OK\r\n"
                     "Content-Type: text/plain; charset=utf-8\r\n\r\n"
                     "Authentication successful. You can close this tab.\r\n"))
    (process-send-eof process)
    (pcase-let ((`(,redirect_uri ,code_verifier) org-gtasks--token-tmp-vars))
      (let* ((resp (request org-gtasks-token-url
                    :type "POST"
                    :data `(("client_id" . ,(org-gtasks-account-client-id account))
                            ("client_secret" . ,(org-gtasks-account-client-secret account))
                            ("code" . ,code)
                            ("redirect_uri" . ,redirect_uri)
                            ("code_verifier" . ,code_verifier)
                            ("grant_type" . "authorization_code"))
                    :parser #'org-gtasks--json-read
                    :sync t))
             (data (request-response-data resp)))
        (when (plist-get data :refresh_token)
          (setf (org-gtasks-account-refresh-token account)
                (plist-get data :refresh_token))
          (org-gtasks--save-refresh-token account)
          (setf (org-gtasks-account-access-token account)
                (plist-get data :access_token))
          (setq org-gtasks--token-tmp-vars nil)
          (message "org-gtasks: authenticated %s"
                   (org-gtasks-account-name account)))))))

;;;; ---- API helpers ----

(defun org-gtasks--api-request (account method url &optional data params)
  "Make a synchronous API request for ACCOUNT.
METHOD is HTTP method string. URL is the full API URL.
DATA is an alist to JSON-encode as body. PARAMS is an alist of query params."
  (let* ((all-params (append `(("access_token" . ,(org-gtasks-account-access-token account)))
                             params))
         (resp (request url
                 :type method
                 :headers (when data '(("Content-Type" . "application/json")))
                 :data (when data (json-encode data))
                 :params all-params
                 :parser #'org-gtasks--json-read
                 :sync t))
         (status (request-response-status-code resp)))
    (cond
     ((eq status 401)
      ;; Token expired — refresh and retry once
      (message "org-gtasks: token expired, refreshing...")
      (setf (org-gtasks-account-access-token account)
            (org-gtasks--get-access-token account))
      (let ((retry-params (append `(("access_token" . ,(org-gtasks-account-access-token account)))
                                  params)))
        (request-response-data
         (request url
           :type method
           :headers (when data '(("Content-Type" . "application/json")))
           :data (when data (json-encode data))
           :params retry-params
           :parser #'org-gtasks--json-read
           :sync t))))
     ((and status (>= status 400))
      (error "org-gtasks: API error %d: %s" status
             (request-response-data resp)))
     (t (request-response-data resp)))))

(defun org-gtasks--get-all-tasks (account tasklist-id)
  "Fetch all tasks from TASKLIST-ID for ACCOUNT, handling pagination."
  (let ((all-tasks nil)
        (page-token nil)
        (keep-going t))
    (while keep-going
      (let* ((params (append '(("maxResults" . "100")
                               ("showCompleted" . "true")
                               ("showHidden" . "true"))
                             (when page-token
                               `(("pageToken" . ,page-token)))))
             (data (org-gtasks--api-request
                    account "GET"
                    (format "%s/lists/%s/tasks" org-gtasks-api-url tasklist-id)
                    nil params))
             (items (plist-get data :items))
             (next (plist-get data :nextPageToken)))
        (when items
          (setq all-tasks (append all-tasks (append items nil))))
        (if (and next (not (string-empty-p next)))
            (setq page-token next)
          (setq keep-going nil))))
    all-tasks))

;;;; ---- Tasklist management ----

(defun org-gtasks--fetch-tasklist-ids (account)
  "Fetch all tasklist name→id mappings for ACCOUNT."
  (let* ((data (org-gtasks--api-request
                account "GET"
                (format "%s/users/@me/lists" org-gtasks-api-url)))
         (items (plist-get data :items)))
    (setf (org-gtasks-account-tasklist-ids account)
          (mapcar (lambda (item)
                    (cons (plist-get item :title) (plist-get item :id)))
                  (append items nil)))))

(defun org-gtasks--ensure-tasklist (account name)
  "Get the tasklist ID for NAME under ACCOUNT, creating it if needed."
  (unless (org-gtasks-account-tasklist-ids account)
    (org-gtasks--fetch-tasklist-ids account))
  (let ((entry (assoc name (org-gtasks-account-tasklist-ids account))))
    (if entry
        (cdr entry)
      ;; Create the tasklist
      (message "org-gtasks: creating tasklist %S..." name)
      (let* ((data (org-gtasks--api-request
                    account "POST"
                    (format "%s/users/@me/lists" org-gtasks-api-url)
                    `(("title" . ,name))))
             (id (plist-get data :id)))
        (push (cons name id) (org-gtasks-account-tasklist-ids account))
        id))))

;;;; ---- Scanning org headings ----

(defun org-gtasks--scan-todos (files)
  "Scan FILES for TODO headings suitable for pushing.
Returns a list of plists with keys:
  :file :point :title :state :deadline :scheduled :gtasks-id :closed :body"
  (let (results)
    (dolist (file files)
      (when (and file (file-exists-p file))
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (re-search-forward org-heading-regexp nil t)
             (let* ((el (org-element-at-point))
                    (todo-state (org-element-property :todo-keyword el))
                    (level (org-element-property :level el)))
               ;; Only top-level or second-level TODOs
               (when (and todo-state (<= level 2))
                 (let* ((title (substring-no-properties
                                (org-element-interpret-data
                                 (org-element-property :title el))))
                        (tags (org-element-property :tags el))
                        (gtasks-id (org-entry-get (point) org-gtasks-gtasks-id-property))
                        (deadline (org-element-property :deadline el))
                        (scheduled (org-element-property :scheduled el))
                        (closed (org-element-property :closed el))
                        ;; Extract body text (notes) excluding properties drawer
                        (contents-begin (org-element-property :contents-begin el))
                        (contents-end (org-element-property :contents-end el))
                        (body (when (and contents-begin contents-end)
                                (save-excursion
                                  (goto-char contents-begin)
                                  (when (re-search-forward org-property-drawer-re contents-end t)
                                    (forward-char))
                                  (let ((start (point)))
                                    (when (< start contents-end)
                                      (string-trim
                                       (buffer-substring-no-properties start contents-end))))))))
                   ;; Only include if tag filter matches (or is disabled)
                   (when (or (null org-gtasks-push-tag)
                             (member org-gtasks-push-tag tags))
                     (push (list :file file
                                 :point (point)
                                 :title title
                                 :state todo-state
                                 :deadline deadline
                                 :scheduled scheduled
                                 :closed closed
                                 :gtasks-id gtasks-id
                                 :body (or body ""))
                           results))))))))))
    (nreverse results)))

;;;; ---- Push: Org → Google Tasks ----

(defun org-gtasks--timestamp-to-iso (ts)
  "Convert an org timestamp element TS to ISO 8601 string, or nil."
  (when ts
    (let ((plist (cadr ts)))
      (org-gtasks--format-org2iso
       (plist-get plist :year-start)
       (plist-get plist :month-start)
       (plist-get plist :day-start)
       (plist-get plist :hour-start)
       (plist-get plist :minute-start)))))

(defun org-gtasks--build-task-payload (todo)
  "Build a Google Tasks API payload from a scanned TODO plist."
  (let ((payload `(("title" . ,(plist-get todo :title))
                   ("status" . ,(if (member (plist-get todo :state) '("DONE" "KILL" "CANCELLED"))
                                    "completed"
                                  "needsAction")))))
    ;; Use deadline or scheduled as the due date
    (when-let ((due (or (org-gtasks--timestamp-to-iso (plist-get todo :deadline))
                        (org-gtasks--timestamp-to-iso (plist-get todo :scheduled)))))
      (push (cons "due" due) payload))
    ;; Add body as notes (truncate to avoid API limits)
    (let ((body (plist-get todo :body)))
      (when (and body (not (string-empty-p body)))
        (push (cons "notes" (substring body 0 (min (length body) 8192))) payload)))
    payload))

(defun org-gtasks--set-heading-property (file point property value)
  "Set PROPERTY to VALUE on the heading at POINT in FILE."
  (with-current-buffer (find-file-noselect file)
    (org-with-wide-buffer
     (goto-char point)
     (org-back-to-heading t)
     (org-set-property property value)
     (save-buffer))))

(defun org-gtasks-push (&optional account)
  "Push TODO headings from org agenda files to Google Tasks.

Scans all files returned by the account's agenda-files-fn for TODO items.
Items with a GTASKS_ID property are updated; new items are created.
Items marked DONE/KILL in org are marked completed in Google Tasks."
  (interactive)
  (let* ((account (or account (org-gtasks--choose-account)))
         (name (org-gtasks-account-name account)))
    (org-gtasks--ensure-token account)
    (let* ((list-id (org-gtasks--ensure-tasklist
                     account (org-gtasks-account-push-tasklist account)))
           (files (if (org-gtasks-account-agenda-files-fn account)
                      (funcall (org-gtasks-account-agenda-files-fn account))
                    (org-agenda-files)))
           (todos (org-gtasks--scan-todos files))
           ;; Fetch existing remote tasks to detect remote completions
           (remote-tasks (org-gtasks--get-all-tasks account list-id))
           (remote-by-id (make-hash-table :test 'equal))
           (local-ids (make-hash-table :test 'equal))
           (pushed 0) (created 0) (completed-remote 0))
      ;; Index remote tasks by ID
      (dolist (rt remote-tasks)
        (puthash (plist-get rt :id) rt remote-by-id))
      ;; Push each local TODO
      (dolist (todo todos)
        (let ((gtasks-id (plist-get todo :gtasks-id))
              (payload (org-gtasks--build-task-payload todo)))
          (if gtasks-id
              ;; Update existing task
              (progn
                (puthash gtasks-id t local-ids)
                (org-gtasks--api-request
                 account "PATCH"
                 (format "%s/lists/%s/tasks/%s" org-gtasks-api-url list-id gtasks-id)
                 payload)
                (cl-incf pushed))
            ;; Only push non-done items as new tasks
            (when (member (plist-get todo :state) org-gtasks-push-todo-states)
              (let* ((resp (org-gtasks--api-request
                            account "POST"
                            (format "%s/lists/%s/tasks" org-gtasks-api-url list-id)
                            payload))
                     (new-id (plist-get resp :id)))
                (when new-id
                  (org-gtasks--set-heading-property
                   (plist-get todo :file) (plist-get todo :point)
                   org-gtasks-gtasks-id-property new-id)
                  (puthash new-id t local-ids)
                  (cl-incf created)))))))
      ;; Check for tasks completed remotely (on phone)
      (dolist (todo todos)
        (let* ((gtasks-id (plist-get todo :gtasks-id))
               (remote (when gtasks-id (gethash gtasks-id remote-by-id)))
               (remote-status (when remote (plist-get remote :status)))
               (local-state (plist-get todo :state)))
          (when (and remote
                     (string= remote-status "completed")
                     (member local-state org-gtasks-push-todo-states))
            ;; Mark as DONE locally
            (with-current-buffer (find-file-noselect (plist-get todo :file))
              (org-with-wide-buffer
               (goto-char (plist-get todo :point))
               (org-back-to-heading t)
               (org-todo "DONE")
               (save-buffer)))
            (cl-incf completed-remote))))
      (message "org-gtasks push [%s]: %d updated, %d created, %d completed from phone"
               name pushed created completed-remote))))

;;;; ---- Pull: Google Tasks → Org inbox ----

(defun org-gtasks-pull (&optional account)
  "Pull new tasks from the Google Tasks inbox list into org inbox.org.

Tasks that already exist in org (matched by GTASKS_ID) are skipped.
New tasks are appended under the Tasks heading in inbox.org."
  (interactive)
  (let* ((account (or account (org-gtasks--choose-account)))
         (name (org-gtasks-account-name account)))
    (org-gtasks--ensure-token account)
    (let* ((list-id (org-gtasks--ensure-tasklist
                     account (org-gtasks-account-pull-tasklist account)))
           (remote-tasks (org-gtasks--get-all-tasks account list-id))
           (inbox-file (org-gtasks-account-inbox-file account))
           (pulled 0))
      (unless inbox-file
        (error "org-gtasks: no inbox-file configured for account %s" name))
      ;; Collect all known GTASKS_IDs and titles from ALL agenda files
      (let* ((files (if (org-gtasks-account-agenda-files-fn account)
                        (funcall (org-gtasks-account-agenda-files-fn account))
                      (list inbox-file)))
             (known-ids (org-gtasks--collect-gtasks-ids-from-files files))
             (known-titles (org-gtasks--collect-todo-titles-from-files files)))
        (dolist (task remote-tasks)
          (let ((task-id (plist-get task :id))
                (title (or (plist-get task :title) ""))
                (status (plist-get task :status))
                (notes (plist-get task :notes))
                (due (plist-get task :due)))
            ;; Skip if: empty title, already have this ID, already have this title, or completed
            (when (and (not (string-empty-p (string-trim title)))
                       (not (member task-id known-ids))
                       (not (member title known-titles))
                       (not (string= status "completed")))
              (org-gtasks--append-to-inbox
               inbox-file task-id title notes due)
              (cl-incf pulled)))))
      (message "org-gtasks pull [%s]: %d new tasks added to inbox" name pulled))))

(defun org-gtasks--collect-gtasks-ids (file)
  "Collect all GTASKS_ID property values from FILE."
  (let (ids)
    (when (file-exists-p file)
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (while (re-search-forward
                 (format ":%s:\\s-+\\(.+\\)" (regexp-quote org-gtasks-gtasks-id-property))
                 nil t)
           (push (string-trim (match-string 1)) ids)))))
    ids))

(defun org-gtasks--collect-gtasks-ids-from-files (files)
  "Collect all GTASKS_ID property values from all FILES."
  (let (all-ids)
    (dolist (file files)
      (when (and file (file-exists-p file))
        (setq all-ids (append all-ids (org-gtasks--collect-gtasks-ids file)))))
    all-ids))

(defun org-gtasks--collect-todo-titles-from-files (files)
  "Collect all TODO heading titles from FILES (for duplicate detection)."
  (let (titles)
    (dolist (file files)
      (when (and file (file-exists-p file))
        (with-current-buffer (find-file-noselect file)
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (re-search-forward org-heading-regexp nil t)
             (let* ((el (org-element-at-point))
                    (todo-state (org-element-property :todo-keyword el)))
               (when todo-state
                 (let ((title (substring-no-properties
                               (org-element-interpret-data
                                (org-element-property :title el)))))
                   (push title titles)))))))))
    titles))

(defun org-gtasks--append-to-inbox (file task-id title notes due)
  "Append a new TODO to FILE with TASK-ID, TITLE, NOTES, and DUE date."
  (with-current-buffer (find-file-noselect file)
    (org-with-wide-buffer
     (goto-char (point-min))
     ;; Find or create Tasks heading
     (unless (re-search-forward "^\\* Tasks" nil t)
       (goto-char (point-max))
       (unless (bolp) (insert "\n"))
       (insert "* Tasks\n"))
     ;; Go to end of Tasks section
     (let ((tasks-end (save-excursion
                        (if (re-search-forward "^\\* " nil t)
                            (match-beginning 0)
                          (point-max)))))
       (goto-char tasks-end)
       (unless (bolp) (insert "\n"))
       ;; Add tag if org-gtasks-push-tag is set
       (let ((tag-str (if org-gtasks-push-tag
                          (format " :%s:" org-gtasks-push-tag)
                        "")))
         (insert (format "** TODO %s%s\n" title tag-str)))
       (when due
         (insert (format "   DEADLINE: <%s>\n" (org-gtasks--format-iso2org due))))
       (insert (format "   :PROPERTIES:\n   :%s: %s\n   :END:\n"
                       org-gtasks-gtasks-id-property task-id))
       (when (and notes (not (string-empty-p (string-trim notes))))
         (insert (format "   %s\n" (string-trim notes))))
       (insert "\n")))
    (save-buffer)))

;;;; ---- Bidirectional sync ----

;;;###autoload
(defun org-gtasks-sync (&optional account)
  "Bidirectional sync: pull from phone inbox, then push org TODOs.
With prefix arg, prompt for account."
  (interactive)
  (let ((account (or account (org-gtasks--choose-account))))
    (org-gtasks-pull account)
    (org-gtasks-push account)
    (message "org-gtasks sync complete for %s" (org-gtasks-account-name account))))

;;;; ---- Cleanup ----

(defun org-gtasks-clear-completed (&optional account)
  "Clear completed tasks from the push tasklist on Google Tasks.
This removes clutter from the phone app without affecting org files."
  (interactive)
  (let* ((account (or account (org-gtasks--choose-account))))
    (org-gtasks--ensure-token account)
    (let ((list-id (org-gtasks--ensure-tasklist
                    account (org-gtasks-account-push-tasklist account))))
      (org-gtasks--api-request
       account "POST"
       (format "%s/lists/%s/clear" org-gtasks-api-url list-id))
      (message "org-gtasks: cleared completed tasks from %s"
               (org-gtasks-account-push-tasklist account)))))

(provide 'org-gtasks)

;;; org-gtasks.el ends here
