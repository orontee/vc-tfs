;;; vc-tfs.el --- support Team Foundation Server for version-control  -*- lexical-binding:t -*-

;; Author: Matthias Meulien <orontee@gmail.com>
;; Package: vc

;; Note that this implementation is based on vc-svn.el.

;; (add-to-list 'vc-handled-backends 'TFS)

;; Todos
;; - simple vc-dir
;; - vc-dir with shelves
;; - Hyper links in history buffers
;; - Rollback

;; Bugs:
;; - vc-diff C-cC-c is not working (not searching for the right file,
;; see diff-find-file-name)
;; - vc-log comment column is truncated

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

(defcustom vc-tfs-diff-switches "/format:context"
  "String or list of strings specifying extra switches for TFS diff under VC.
If nil, use the value of `vc-diff-switches' (or `diff-switches'),
together with \"/format:context\" to force the use of the UNIX
based 'diff -c' output format. If you want to force an empty list
of arguments, use t."
  :type '(choice (const :tag "Unspecified" nil)
		 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List"
			 :value ("")
			 string))
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

;; TODO Prepare for internationalization

(defun vc-tfs-checkout-model (_files) 'announce)

;;;
;;; State-changing functions
;;;

(defun vc-tfs-create-repo (backend)
  "Create a workspace and a workspace mapping."
  ;;  (vc-tfs-command "*vc*" 0 "workspace" '("/new" "/noprompt"))
  ;; Use workfold to create a mapping
  (error "Not yet implemented"))

(defun vc-tfs-register (files &optional rev comment)
  "Register FILES into the TFS version-control system.  
The REV and COMMENT arguments are ignored."
  (apply 'vc-tfs-command nil 0 files "add" (vc-switches 'TFS 'register)))

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
	    (list "/noprompt")
	    (and rev (not (string= rev ""))
		(list (concat "/version:C" rev)))
	    (vc-switches 'TFS 'checkout)))))

;; TODO Check that the switch is well computed

(defun vc-tfs-checkout (file &optional editable rev)
  ;; TODO Don't recreate file when existing with required version
  (apply 'vc-tfs-command nil 0 file "get"
	   (append
	    (list "/noprompt")
	    (cond
	     ((null rev) (list "/version:T"))
	     ((or (eq rev t) (equal rev "")) nil)
	     (t (list (concat "/version:C" rev))))))
  (vc-tfs-command nil 0 file "checkout"))

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
		    (when limit (list (format "/stopafter:%s" limit))))))
	;; Dump log for the entire directory.
	(apply 'vc-tfs-command buffer 0 (list ".") "history"
	       (append
		(list
		 (if start-revision (format "/version:C%s" start-revision) "/version:T"))
		(list "/noprompt")
		(when limit (list (format "/stopafter:%s" limit)))))))))

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
	      (list "/format:Context"
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
