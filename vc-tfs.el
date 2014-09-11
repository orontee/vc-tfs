;;; vc-tfs.el --- support Team Foundation Server for version-control  -*- lexical-binding:t -*-

;; Author: Matthias Meulien <orontee@gmail.com>
;; Package: vc

;; Note that this implementation is based on vc-svn.el.

(add-to-list 'vc-handled-backends 'TFS)

;; Todos
;; - vc-update (must implement merge-news)
;; - vc-root-diff
;; - vc-print-root-log
;; - vc-dir with shelves
;; - Short and long format for logs
;; - Rollback
;; - Workspace in `help-echo' property of mode line string
;; - Revision completion
;; - Delete, rename files

;; Bugs:
;; - vc-diff C-cC-c is not working (not searching for the right file,
;; see diff-find-file-name)
;; - diff or log on selected files in vc-dir
;; - Characters encoding in log buffers
;; - Output parsing is dependant on locale
;; - Check logs, missing /version:W

;;; Code:

(eval-when-compile
  (require 'vc))

;; Clear up the cache to force vc-call to check again and discover
;; new functions when we reload this file.
(put 'TFS 'vc-functions nil)

;;;
;;; Customization options
;;;

(defgroup vc-tfs nil
  "VC Team Foundation Server (TFS) backend."
  :version "24.4"
  :group 'vc)

(defcustom vc-tfs-program "tf"
  "Name of the TFS executable."
  :type 'string
  :group 'vc-tfs)

(defcustom vc-tfs-global-switches nil
  "Global switches to pass to any TFS command."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
  :version "24.4"
  :group 'vc-tfs)

(defcustom vc-tfs-register-switches nil
  "Switches for registering a file into TFS.
A string or list of strings passed to the checkin program by
\\[vc-register].  If nil, use the value of `vc-register-switches'.
If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
		 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :version "24.4"
  :group 'vc-tfs)

(defcustom vc-tfs-diff-switches "/format:Unified"
  "String or list of strings specifying extra switches for TFS diff under VC.
If nil, use the value of `vc-diff-switches' (or `diff-switches'),
together with \"/format:Unified\" to force the use of the UNIX
based 'diff -u' output format. If you want to force an empty list
of arguments, use t."
  :type '(choice (const :tag "Unspecified" nil)
		 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :version "24.4"
  :group 'vc-tfs)

;;;

(defvar vc-tfs-brief-log-format
  '("^\\(?1:[0-9]+\\) +\\(?:[a-z,]+\\) +\\(?2:.\\{17\\}\\) +\\(?3:.\\{10\\}\\) +\\(?4:.+\\)"
    (1 'log-view-message-face)
    (2 'change-log-name)
    (3 'change-log-date)))

;;; Properties of the backend

(defun vc-tfs-revision-granularity () 'repository)

;;;
;;; State-querying functions
;;;

;;;###autoload (defun vc-tfs-registered (file)
;;;###autoload  "Return non-nil if FILE is registered with TFS."
;;;###autoload  (load "vc-tfs" nil t)
;;;###autoload  (vc-tfs-registered file))

(defun vc-tfs-registered (file)
  "Check whether FILE is registered with TFS."
  (with-temp-buffer
    (cd (file-name-directory file))
    (let* (process-file-side-effects
	   (status
	    (condition-case nil
		;; Ignore all errors.
		(vc-tfs-command t t file "localversions")
	      (error nil))))
      (eq 0 status))))

;; TODO Why not use something as simple as vc-tfs-responsible-p?

;; (defun vc-tfs-root (file)
;;   "Return the root of the hierarchy for FILE."
;;   (with-temp-buffer
;;     (let ((dir (or (and (file-directory-p file) file) (file-name-directory file)))
;; 	  (re " $/.+: \\(.+\\)"))
;;       (cd dir)
;;       (vc-tfs-command t 0 dir "workspaces")
;;       (re-search-forward re)

(defun vc-tfs-state (file)
  "TFS-specific function to compute the version control state."
  (if (not (vc-tfs-registered file))
      'unregistered
    (with-temp-buffer
      (cd (file-name-directory file))
      (vc-tfs-command t 0 file "properties")
      (vc-tfs-parse-properties))))

(defun vc-tfs-working-revision (file)
  "TFS-specific function to compute the working revision of FILE."
    (with-temp-buffer
      (cd (file-name-directory file))
      (vc-tfs-command t 0 file "localversions")
      (goto-char (point-min))
      (cond
       ((re-search-forward (concat
			    (regexp-quote (file-name-nondirectory file))
			    ";C\\([0-9]+\\)")
			   nil t)
	(match-string 1))
       (t "0"))))

(defun vc-tfs-parse-properties ()
  "Parse output of \"tf properties\" command in the current buffer."
  (goto-char (point-min))
  (let ((registered
	 (re-search-forward
	  (concat "Local information:" (string ?\n)
		  "\\(?:.*" (string ?\n) "\\)\\{2\\}"
		  "\\(?:  Changeset +: ?\\([0-9]*\\)\\)" (string ?\n)
		  "\\(?:  Change +: \\([^ ]+\\)\\)" (string ?\n)
		  ".*" (string ?\n)
		  "Server information:" (string ?\n)
		  ".*" (string ?\n)
		  "\\(?:  Changeset +: ?\\([0-9]*\\)\\)" (string ?\n)
		  ".*" (string ?\n)
		  "\\(?:  Lock +: \\([^ ]+\\)\\)" (string ?\n)
		  "\\(?:  Lock owner +: ?\\(.*\\)\\)")
	  nil t)))
    (cond
     (registered
      (let ((change (match-string 2))
	    (obsolete (not (equal (match-string 1) (match-string 3))))
	    (lock (match-string 4))
	    (lock-owner (match-string 5)))
	(cond
	 ((not (equal lock "none")) lock-owner)
	 ((equal change "edit")
	  (if obsolete 'needs-merge 'edited))
	 ((equal change "none")
	  (if obsolete 'needs-update 'up-to-date))
	 ((equal change "add") 'added)
	 ((equal change "deleted") 'removed)
	 ((equal change "conflict") 'conflict)
	 ((equal change "missing") 'missing)
	 (t (error (concat "Not implemented state: " change))))))
     (t 'unregistered))))

;; TODO Does TFS handles ignored files?

(defun vc-tfs-checkout-model (_files) 'announce)

;;;
;;; State-changing functions
;;;

(defun vc-tfs-create-repo (backend)
  "Create a workspace and a workspace mapping."
  ;; (vc-tfs-command "*vc*" 0 name "workspace" '("/new" "/noprompt" "/collection:"))
  ;; (vc-tfs-command "*vc*" 0 nil "workfold" '("/map $/ ." ))
  (error "Not yet implemented"))

(defun vc-tfs-register (files &optional rev comment)
  "Register FILES into the TFS version-control system.  
The REV and COMMENT arguments are ignored."
  (apply 'vc-tfs-command nil 0 files "add" (vc-switches 'TFS 'register)))

(defun vc-tfs-responsible-p (file)
  (let ((status (vc-tfs-command nil 1 file "localversions")))
    (equal status 0)))

(defun vc-tfs-checkin (file rev comment)
  "Commit changes in FILES into the TFS version-control system."
  (let ((status (apply 'vc-tfs-command nil 1 files "checkin"
		       (nconc (list "/comment" comment) (vc-switches 'TFS 'checkin)))))
    (set-buffer "*vc*")
    (goto-char (point-min))
    (unless (equal status 0)
      (error "Check-in failed"))))

;; TODO Check failure reason to suggest merge

(defun vc-tfs-find-revision (file rev buffer)
  "Fetch revision REV of file FILE from the TFS version-control
system.

If REV is the empty string, fetch the revision of the workspace."
  (let (process-file-side-effects)
    (apply 'vc-tfs-command
	   buffer 0 file "view"
	   (nconc
	    (and rev (not (string= rev ""))
		 (list (concat "/version:C" rev)))
	    (list "/noprompt")))))

(defun vc-tfs-checkout (file &optional editable rev)
  ;; TODO Don't recreate file when existing with required version
  (apply 'vc-tfs-command nil 0 file "get"
	   (append
	    (list "/noprompt")
	    (cond
	     ((null rev) (list "/version:T"))
	     ((or (eq rev t) (equal rev "")) nil)
	     (t (list (concat "/version:C" rev))))))
  (vc-tfs-command nil 0 file "checkout" (vc-switches 'TFS 'checkout)))

(defun vc-tfs-revert (file &optional contents-done)
  "Removes pending changes from the workspace for FILE."
  (unless contents-done
    (vc-tfs-command nil 0 file "undo")))

;; TODO Check that if the file is in the `added' state it is returned
;; back to the `unregistered' state

;;;
;;; History functions
;;;

(defun vc-tfs-print-log (files buffer &optional shortlog start-revision limit)
    "Print commit log associated with FILES into specified BUFFER.
SHORTLOG and START-REVISION are ignored.
If LIMIT is non-nil, show no more than this many entries."
  (save-current-buffer
    (vc-setup-buffer buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (if files
	  (dolist (file files)
		  (insert "Working file: " file "\n")
		  (apply
		   'vc-tfs-command
		   buffer
		   'async
		   (list file)
		   "history"
		   (append
		    (list
		     (if start-revision
			 (format "/version:C%s" start-revision)
		       "/version:T"))
		    (list "/noprompt")
		    (when (not shortlog) (list "/format:detailed"))
		    (when limit (list (format "/stopafter:%s" limit))))))
	(apply 'vc-tfs-command buffer 0 (list ".") "history"
	       (append
		(list
		 (if start-revision (format "/version:C%s" start-revision) "/version:T"))
		(list "/noprompt")
		(when (not shortlog) (list "/format:detailed"))
		(when limit (list (format "/stopafter:%s" limit)))))))))

;; TODO Throw an error for filesets; Not supported

(defun vc-tfs-log-incoming (buffer remote-location)
  (apply 'vc-tfs-command buffer 0 default-directory "history"
	 (append (list "/noprompt")
		 (list "/version:W~T") (list "/recursive"))))

(defvar log-view-message-re)
(defvar log-view-file-re)
(defvar log-view-font-lock-keywords)
(defvar log-view-per-file-logs)
(defvar log-view-expanded-log-entry-function)

(define-derived-mode vc-tfs-log-view-mode log-view-mode "TFS-Log-View"
  (require 'add-log)
  (set (make-local-variable 'log-view-file-re)
       "^Working file: \\(.+\\)")
  (set (make-local-variable 'log-view-per-file-logs) nil)
  (set (make-local-variable 'log-view-message-re)
       (if (not (eq vc-log-view-type 'long))
	   (car vc-tfs-brief-log-format)
	 "^Changeset: \\([0-9]+\\)"))
  (when (eq vc-log-view-type 'short)
    (setq truncate-lines t)
    (set (make-local-variable 'log-view-expanded-log-entry-function)
	 'vc-tfs-expanded-log-entry))
  (set (make-local-variable 'log-view-font-lock-keywords)
       (if (not (eq vc-log-view-type 'long))
	   (list vc-tfs-brief-log-format)
	 (append
	  `((,log-view-message-re (1 'change-log-acknowledgment))
	    (,log-view-file-re (1 'change-log-file)))
	  '(("^User: \\(.+\\)" (1 'change-log-name))
	    ("^Date: \\(.+\\)" (1 'change-log-date)))))))

(defun vc-tfs-expanded-log-entry (revision)
  (with-temp-buffer
    (apply 'vc-tfs-command t nil nil "changeset"
	   (append (list revision)
		   (list "/noprompt")))
    (goto-char (point-min))
    (unless (eobp)
      (indent-region (point-min) (point-max) 2)
      (buffer-string))))

(defun vc-tfs-diff (files &optional oldvers newvers buffer)
  "Get a difference report using TFS between two revisions of fileset FILES."
  (and oldvers
       (not newvers)
       files
       (catch 'no
	 (dolist (f files)
	   (or (equal oldvers (vc-working-revision f))
	       (throw 'no nil)))
	 t)
       ;; Use nil rather than the current revision because tfs handles
       ;; it better (i.e. locally).  Note that if _any_ of the files
       ;; has a different revision, we fetch the lot, which is
       ;; obviously sub-optimal.
       (setq oldvers nil))
  (let* ((switches
	    (if vc-tfs-diff-switches
		(vc-switches 'TFS 'diff)
	      (list "/format:Unified"
		    (mapconcat 'identity (vc-switches nil 'diff) " "))))
	   (async (and (not vc-disable-async-diff)
                       (vc-stay-local-p files 'TFS))))
      (apply 'vc-tfs-command buffer
	     (if async 'async 0)
	     files "diff"
	     (append
	      switches
	      (when oldvers
		(list "/version:C" (if newvers (concat oldvers "~C" newvers)
			     oldvers)))))
      (if async 1		      ; async diff => pessimistic assumption
	;; For some reason `tfs diff' does not return a useful
	;; status w.r.t whether the diff was empty or not.
	(buffer-size (get-buffer buffer)))))

;; TODO Throw an error for filesets; Not supported

;;;
;;; Directory functions
;;;

(defun vc-tfs-after-dir-status (callback)
  (let ((state-map '(("add" . added)
		     ("edit" . edited)))	; TODO Find other values
	(re "^\\(.*\\) +\\(edit\\|add\\) +\\(.*\\)$")
	result)
    (goto-char (point-min))
    (while (re-search-forward re nil t)
      (let ((state (cdr (assoc (match-string 2) state-map)))
	    (filename (file-relative-name
		       (replace-regexp-in-string "\\\\" "/" (match-string 3))
		       default-directory)))
	(setq result (cons (list filename state) result))))
    (funcall callback result)))

;; TODO How to list unknown files. Should I use properties?

;; TODO Should we use the detailed format to prevent troubles with
;; fixed length columns

(declare-function vc-exec-after "vc-dispatcher" (code))

(defun vc-tfs-dir-status (dir callback)
  "Return a list build from status of files in DIR."
  (vc-tfs-command (current-buffer) 'async dir "status" "/recursive")
  (vc-run-delayed
    (vc-tfs-after-dir-status callback)))

;; TODO vc-tfs-dir-status-files

;; TODO vc-tfs-dir-extra-headers for workspace root

;;;
;;; Internal functions
;;;

(defun vc-tfs-command (buffer okstatus file-or-list &rest flags)
  "A wrapper around `vc-do-command' for use in vc-tfs.el.
The difference to vc-do-command is that this function always invokes `tfs',
and that it passes `vc-tfs-global-switches' to it before FLAGS."
  (apply 'vc-do-command (or buffer "*vc*") okstatus vc-tfs-program file-or-list
         (if (stringp vc-tfs-global-switches)
             (cons vc-tfs-global-switches flags)
           (append vc-tfs-global-switches flags))))

(provide 'vc-tfs)

;;; vc-tfs.el ends here
