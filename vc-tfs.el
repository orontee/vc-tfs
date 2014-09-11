;;; vc-tfs.el --- support Team Foundation Server for version-control  -*- lexical-binding:t -*-

;; Author: Matthias Meulien <orontee@gmail.com>
;; Package: vc

;; Note that most of this implementation was taken from vc-svn.el and
;; vc-git.el.

;; Todos:
;; - vc-update (must implement merge-news)
;; - vc-root-diff
;; - vc-print-root-log
;; - Shelves support (shelve, view, unshelve, delete, list)
;; - Rollback
;; - Revision completion
;; - Change comment modification (modify-change-comment)
;; - Delete, rename files
;; - Other todos can be found in the source code

;; Bugs:
;; - vc-diff C-cC-c is not working (not searching for the right file,
;; see diff-find-file-name)
;; - Characters encoding in log buffers
;; - Output parsing is dependant on language (bind locally `process-environment')

;;; Code:

(eval-when-compile
  (require 'vc))

(add-to-list 'vc-handled-backends 'TFS)

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

;; Perform a status of current directory, read Workspace name from
;; there, then run workspaces command with that name and detailed
;; format to get the mapping

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
	 ;; TODO Complete with other values of `change'
	 (t (error (concat "Not implemented state: " change))))))
     (t 'unregistered))))

(defun vc-tfs-checkout-model (_files)
   ;; TODO This should be dependent on host properties
  'announce)

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

(defalias 'vc-tfs-responsible-p 'vc-tfs-registered)

;; TODO We'd better check if file is a descendant of a workspace
;; root

(defun vc-tfs-checkin (file rev comment)
  "Commit changes in FILES into the TFS version-control system."
  (let ((status (apply 'vc-tfs-command nil 1 files "checkin"
		       (nconc (list "/comment" comment)
			      (vc-switches 'TFS 'checkin)))))
    (set-buffer "*vc*")
    (goto-char (point-min))
    (unless (equal status 0)
      ;; TODO Check failure reason to suggest merge
      (error "Check-in failed"))))

(defun vc-tfs-find-revision (file rev buffer)
  "Fetch revision REV of file FILE from the TFS version-control
system.

If REV is the empty string, fetch the revision of the workspace."
  (let (process-file-side-effects)
    (apply 'vc-tfs-command
	   buffer 0 file "view"
	   (nconc
	    `(,(if (and rev (not (string= rev ""))
			(concat "/version:C" rev))
		   "/version:W"))
	    (list "/noprompt")))))

(defun vc-tfs-checkout (file &optional editable rev)
  ;; TODO Don't recreate file if it already exists with required
  ;; version
  (apply 'vc-tfs-command nil 0 file "get"
	   (append
	    (cond
	     ((null rev) (list "/version:W"))
	     ((or (eq rev t) (equal rev "")) "/version:T")
	     (t (list (concat "/version:C" rev))))
	    (list "/noprompt")))
  (vc-tfs-command nil 0 file "checkout" (vc-switches 'TFS 'checkout)))

(defun vc-tfs-revert (file &optional contents-done)
  "Removes pending changes from the workspace for FILE."
  (unless contents-done
    (vc-tfs-command nil 0 file "undo")))

;;;
;;; History functions
;;;

(defun vc-tfs-print-log (files buffer &optional shortlog start-revision limit)
    "Print commit log associated with FILES into specified BUFFER.
SHORTLOG and START-REVISION are ignored.
If LIMIT is non-nil, show no more than this many entries."
  (save-current-buffer
    (vc-setup-buffer buffer)
    (let ((inhibit-read-only t)
	  (file (if (listp files)
		    (if (eq (length files) 1)
			(car files)
		      (error "Fileset log is not supported"))
		  files)))
      (goto-char (point-min))
      (apply 'vc-tfs-command buffer 'async (list file) "history"
	     (append
	      (list
	       (if start-revision
		   (format "/version:C%s" start-revision)
		 "/version:W"))
	      (when (not shortlog) (list "/format:detailed"))
	      (when limit (list (format "/stopafter:%s" limit)))
	      (list "/noprompt"))))))

(defun vc-tfs-log-incoming (buffer remote-location)
  (apply 'vc-tfs-command buffer 0 default-directory "history"
	 (append (list "/version:W~T")
		 (list "/recursive")
		 (list "/noprompt"))))

(defvar log-view-message-re)
(defvar log-view-file-re)
(defvar log-view-font-lock-keywords)
(defvar log-view-per-file-logs)
(defvar log-view-expanded-log-entry-function)

;; Surprisingly, the command "tf history" changes its output format
;; according to the "/recursive" option

(defvar vc-tfs-brief-log-formats
  '(default
     "^\\(?1:[0-9]+\\) +\\(?:[a-z,]+\\) +\\(?2:.\\{17\\}\\) +\\(?3:.\\{10\\}\\) +\\(?4:.+\\)"
     recursive
     "^\\(?1:[0-9]+\\) +\\(?2:.\\{17\\}\\) +\\(?3:.\\{10\\}\\) +\\(?4:.+\\)"))

(defun vc-tfs-guess-brief-log-format ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^Changeset +Change " nil t 1)
    (let ((type (if (eq (match-string 0) nil) 'recursive 'default)))
      (cons
       (plist-get vc-tfs-brief-log-formats type)
       '((1 'log-view-message-face)
	 (2 'change-log-name)
	 (3 'change-log-date))))))

(define-derived-mode vc-tfs-log-view-mode log-view-mode "TFS-Log-View"
  (require 'add-log)
  (let ((brief-log-format
	 (and (not (eq vc-log-view-type 'long))
	      (vc-tfs-guess-brief-log-format))))
    (set (make-local-variable 'log-view-file-re) "\\`a\\`")
    (set (make-local-variable 'log-view-per-file-logs) nil)
    (set (make-local-variable 'log-view-message-re)
	 (if brief-log-format
	     (car brief-log-format)
	   "^Changeset: \\([0-9]+\\)"))
    (when (eq vc-log-view-type 'short)
      (setq truncate-lines t)
      (set (make-local-variable 'log-view-expanded-log-entry-function)
	   'vc-tfs-expanded-log-entry))
    (set (make-local-variable 'log-view-font-lock-keywords)
	 (if brief-log-format
	     (list brief-log-format)
	   (append
	    `((,log-view-message-re (1 'change-log-acknowledgment)))
	    '(("^User: \\(.+\\)" (1 'change-log-name))
	      ("^Date: \\(.+\\)" (1 'change-log-date))))))))

(defun vc-tfs-expanded-log-entry (revision)
  (with-temp-buffer
    (apply 'vc-tfs-command t nil nil "changeset"
	   (append (list revision)
		   (list "/noprompt")))
    (goto-char (point-min))
    (unless (eobp)
      (indent-region (point-min) (point-max) 2)
      (buffer-string))))

;; TODO Optionally Remove empty lines and useless info

(defun vc-tfs-diff (files &optional oldvers newvers buffer)
  "Get a difference report using TFS between two revisions of fileset FILES."
  (let ((file (if (listp files)
		  (if (eq (length files) 1)
		      (car files)
		    (error "Fileset log is not supported"))
		files))		;TODO Could iterate through files
	(switches
	 (if vc-tfs-diff-switches
	     (vc-switches 'TFS 'diff)
	   (nconc (list "/format:Unified")
		  (vc-switches nil 'diff)))))
    (apply 'vc-tfs-command buffer 'async file "diff"
	   (append
	    switches
	    (list 
	     (if oldvers
		 (concat "/version:C"
			 (if newvers (concat oldvers "~C" newvers)
			   oldvers))
	       (concat "/version:W" (when newvers "~C" newvers)))))))
  1)

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
