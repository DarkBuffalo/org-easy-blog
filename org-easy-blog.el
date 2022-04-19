;;; org-easy-blog.el --- Write blog easy -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Matthias David
;;
;; Author: Matthias David <https://github.com/DarkBuffalo>
;; Maintainer: Matthias David <darkbuffalo@delta.re>
;; Created: mars 26, 2021
;; Modified: mars 26, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/DarkBuffalo/org-easy-blog
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Write blog easy
;;
;;; Code:

(require 'cl-lib)

(defcustom org-easy-blog-url nil
  "Url of the site."
  :group 'org-easy-blog
  :type 'string)

(defcustom org-easy-blog-basedir nil
  "Directory where html source code is placed."
  :group 'org-easy-blog
  :type 'string)

(defcustom org-easy-blog-postdir "content/posts"
  "Directory where the theme stores its posts."
  :group 'org-easy-blog
  :type 'string)

(defcustom org-easy-blog-sort-default-char nil
  "Default setting to sort with charactor."
  :group 'org-easy-blog
  :type 'integer)

(defcustom org-easy-blog-no-help nil
  "No help flg of `org-easy-blog'."
  :group 'org-easy-blog
  :type 'integer)

(defcustom org-easy-blog-help-line 7
  "Number of lines of `org-easy-blog-help'."
  :group 'org-easy-blog
  :type 'integer)

(defcustom org-easy-blog-additional-help nil
  "Additional help flg of `org-easy-blog'."
  :group 'org-easy-blog
  :type 'integer)

(defconst org-easy-blog--buffer-name "*org-easy-blog*"
  "Buffer name of `org-easy-blog'.")

(defconst org-easy-blog--formats `("org"))

(defconst org-easy-blog--forward-char 20
  "Forward-char of `org-easy-blog'.")

(defface org-easy-blog-help-face
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :bold t
     :foreground "#82c600"
     :background "#f0f8ff")
    (((class color) (background dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :bold t
     :foreground "#82c600"
     :background "#2f4f4f"))
  "Definition of help color."
  :group 'org-easy-blog-faces)

(defvar org-easy-blog--mode-buffer nil
  "Main buffer of `org-easy-blog'.")

(defvar org-easy-blog--cursor nil
  "Cursor of `org-easy-blog'.")

(defvar org-easy-blog--refresh nil
  "Refresh flg of `org-easy-blog'.")

(defvar org-easy-blog--draft-list nil
  "Draft list flg.")

(defvar org-easy-blog--sort-char-flg nil
  "Sort char flg of `org-easy-blog'.")

(defvar org-easy-blog--sort-publishday-flg nil
  "Sort publishtime flg of `org-easy-blog'.")

(defvar org-easy-blog--sort-time-flg 1
  "Sort time flg of `org-easy-blog'.")

(if org-easy-blog-no-help
    (defvar org-easy-blog--unmovable-line 3
      "Impossible to move below this line.")
  (defvar org-easy-blog--unmovable-line (+ org-easy-blog-help-line 4)
    "Impossible to move below this line."))

(defmacro org-easy-blog-with-env (&rest body)
  "Evaluate BODY with `default-directory' set to `org-easy-blog-basedir'.
Report an error if `org-easy-blog-basedir' is unset."
  `(progn
     (unless org-easy-blog-basedir
       (error "Please set org-easy-blog-basedir variable"))
     (let ((default-directory org-easy-blog-basedir))
       ,@body)))

(defmacro org-easy-blog-ignore-error (condition &rest body)
  "Execute BODY; if the error CONDITION occurs, return nil.
Otherwise, return result of last form in BODY.
CONDITION can also be a list of error conditions."
  (declare (debug t) (indent 1))
  `(condition-case nil (progn ,@body) (,condition nil)))

(defcustom org-easy-blog-emacspeak nil
  "Emacspeak flg of `org-easy-blog'."
  :group 'org-easy-blog
  :type 'integer)

(defcustom org-easy-blog-default-ext ".org"
  "Default extension when posting new articles."
  :group 'org-easy-blog
  :type 'string)

(defcustom org-easy-blog-bloglist nil
  "Multiple blog setting."
  :group 'org-easy-blog
  :type 'string)

(push `((org-easy-blog-basedir . ,org-easy-blog-basedir)
	(org-easy-blog-url . ,org-easy-blog-url)
	;;(org-easy-blog-root . ,org-easy-blog-root)
	;;(org-easy-blog-sshdomain . ,org-easy-blog-sshdomain)
	(org-easy-blog-postdir . ,org-easy-blog-postdir)
	(org-easy-blog-sort-default-char . ,org-easy-blog-sort-default-char)
	(org-easy-blog-default-ext . ,org-easy-blog-default-ext))
      org-easy-blog-bloglist)

;; SERVE
(defvar org-easy-blog--server-process nil
  "Server process.")

(defvar org-easy-blog--local-port
  "3000"
  "Port to serve local blog instance.")

;;;###autoload
(defun org-easy-blog-preview ()
  "Preview at localhost."
  (interactive)
  (let ((default-directory (expand-file-name org-easy-blog-basedir)))	;;change to public dir 
    (setq org-easy-blog--server-process
          (start-process "org-easy-blog--server-process" nil "python" "-m" "http.server" (format "%s" org-easy-blog--local-port)))
    (browse-url (format "http://localhost:%s" org-easy-blog--local-port))))

(defun org-easy-blog-preview-end ()
  "Finish previewing hugo at localhost."
  (unless (null org-easy-blog--server-process)
    (delete-process org-easy-blog--server-process))
  ;;(when (get-buffer easy-hugo--preview-buffer)
  ;;  (kill-buffer easy-hugo--preview-buffer))
  )


(defcustom org-easy-blog-help
  (if (null org-easy-blog-sort-default-char)
      (progn
	"n .. New blog post    R .. Rename file    D .. Draft list
p .. Preview          g .. Refresh       u .. Sort publishday
v .. Open view-mode   s .. Sort time     T .. Publish timer    N .. No help-mode
d .. Delete post      c .. Open config   W .. AWS S3 timer     f .. Select filename
P .. Publish clever   C .. Deploy GCS    a .. Search with ag   H .. GitHub timer
< .. Previous blog    > .. Next blog     , .. Pre postdir      . .. Next postdir
/ .. Select postdir   q .. Quit easy-blog
")
    (progn
      "n .. New blog post    R .. Rename file    D .. Draft list
p .. Preview          g .. Refresh       u .. Sort publishday
v .. Open view-mode   s .. Sort char     T .. Publish timer    N .. No help-mode
d .. Delete post      c .. Open config   ; .. Select blog      f .. Select filename
P .. Publish clever   C .. Deploy GCS    a .. Search with ag   H .. GitHub timer
< .. Previous blog    > .. Next blog     , .. Pre postdir      . .. Next postdir
/ .. Select postdir   q .. Quit easy-blog
"))
  "Help of `org-easy-blog'."
  :group 'org-easy-blog
  :type 'string)


(defconst org-easy-blog--first-help
  "Welcome to org-easy-blog
Let's post an article first.
Press n on this screen or M-x easy-hugo-newpost.
Enter a article file name in the minibuffer.
Then M-x easy-hugo again or refresh the screen with r or g key in this buffer,
article which you wrote should appear here.
Enjoy!
"
  "Help of `org-easy-blog' first time.")

(defcustom org-easy-blog-add-help
  (if (null org-easy-blog-sort-default-char)
      (progn
	"O .. Open basedir     r .. Refresh       b .. X github timer   t .. X publish-timer
k .. Previous-line    j .. Next line     h .. backward-char    l .. forward-char
m .. X s3-timer       i .. X GCS timer   I .. GCS timer        V .. View other window
- .. Pre postdir      + .. Next postdir  w .. Write post       o .. Open other window
J .. Jump blog        e .. Edit file     B .. Firebase deploy  ! .. X firebase timer
L .. Firebase timer   S .. Sort char     M .. Magit status     ? .. Describe-mode
")
    (progn
      "O .. Open basedir     r .. Refresh       b .. X github timer   t .. X publish-timer
k .. Previous-line    j .. Next line     h .. backward-char    l .. forward-char
m .. X s3-timer       i .. X GCS timer   I .. GCS timer        V .. View other window
- .. Pre postdir      + .. Next postdir  w .. Write post       o .. Open other window
J .. Jump blog        e .. Edit file     B .. Firebase deploy  ! .. X firebase timer
L .. Firebase timer   S .. Sort time     M .. Magit status     ? .. Describe-mode
"))
  "Add help of `org-easy-blog'."
  :group 'org-easy-blog
  :type 'string)

(defvar org-easy-blog-mode-map
  (let ((map (make-keymap)))
    (define-key map "n" 'org-easy-blog-newpost)
    (define-key map "q" 'org-easy-blog-quit)
    (define-key map [backtab] 'org-easy-blog-no-help)
    (define-key map "r" 'easy-hugo-refresh)
    (define-key map "p" 'org-easy-blog-preview)

    map)
  "Keymap for `org-easy-blog' major mode.")

(defun org-easy-blog-quit ()
  "Quit easy blog."
  (interactive)
  (setq org-easy-blog--sort-time-flg 1)
  (setq org-easy-blog--sort-char-flg nil)
  (org-easy-blog-preview-end)
  (when (buffer-live-p org-easy-blog--mode-buffer)
    (kill-buffer org-easy-blog--mode-buffer)))


(define-derived-mode org-easy-blog-mode special-mode "org-easy-blog"
  "Major mode for easy hugo.")

(defsubst org-easy-blog--directory-name-p (name)
  "Return non-nil if NAME ends with a directory separator character."
  (let ((len (length name))
        (lastc ?.))
    (if (> len 0)
        (setq lastc (aref name (1- len))))
    (or (= lastc ?/)
        (and (memq system-type '(windows-nt ms-dos))
             (= lastc ?\\)))))

(defun org-easy-blog-emacspeak-filename ()
  "Read filename with emacspeak."
  (cl-declare (special emacspeak-speak-last-spoken-word-position))
  (let ((filename (substring (thing-at-point 'line) org-easy-blog--forward-char -1))
        (personality (dtk-get-style)))
    (cond
     (filename
      (dtk-speak (propertize filename 'personality personality))
      (setq emacspeak-speak-last-spoken-word-position (point)))
     (t (emacspeak-speak-line)))))

(defun org-easy-blog--directory-files (dir regexp)
  "Return list of all files under DIR that have file names matching REGEXP."
  (let ((result nil)
	(files nil)
	(tramp-mode (and tramp-mode (file-remote-p (expand-file-name dir)))))
    (dolist (file (sort (file-name-all-completions "" dir)
			'string<))
      (unless (member file '("./" "../"))
	(if (not (org-easy-blog--directory-name-p file))
	    (when (string-match regexp file)
	      (push (expand-file-name file dir) files)))))
    (nconc result (nreverse files))))

(defun org-easy-blog--publishday-alist ()
  "Return article alist with publishing date."
  (let* ((files (org-easy-blog--directory-files
		 (expand-file-name
		  org-easy-blog-postdir
		  org-easy-blog-basedir)
		 ""))
	 (filelist files)
	 (result (list)))
    (let ((source (with-temp-buffer
		    (while files
		      (insert-file-contents (car files))
		      (pop files))
		    (buffer-string))))
      (save-match-data
	(let ((pos 0)
	      matches)
	  (while (string-match "^[D\\|d]ate[:]? [=]?+[ ]*\\(.+?\\)$" source pos)
	    (push (match-string 1 source) matches)
	    (setq pos (match-end 0)))
	  (when matches
	    (let ((timestamplist
		   (delete "" (split-string
			       (replace-regexp-in-string
				"[\"\']" " "
				(replace-regexp-in-string "[,()]" "" (format "%s" matches)))
			       " "))))
	      (while timestamplist
		(push (cons (car timestamplist) (car filelist)) result)
		(pop timestamplist)
		(pop filelist))
	      result)))))))

(defun org-easy-blog--orgtime-format (x)
  "Format orgtime as X."
  (concat (substring x 0 3) ":" (substring x 3 5)))


(defun org-easy-blog--org-headers (file)
  "Return a draft org mode header string for a new article as FILE."
  (let ((datetimezone
         (concat
          (format-time-string "%Y-%m-%dT%T")
          (org-easy-blog--orgtime-format (format-time-string "%z")))))
    (concat
     "#+TITLE: " file
     "\n#+DATE: " datetimezone
     "\n#+PUBLISHDATE: " datetimezone
     "\n#+DRAFT: nil"
     "\n#+TAGS: nil, nil"
     "\n#+DESCRIPTION: Short description"
     "\n\n")))

;;;###autoload
(defun org-easy-blog-newpost (post-file)
  "Create a new post with hugo.
POST-FILE needs to have and extension  '.org'."
  (interactive (list (read-from-minibuffer
		      "Filename: "
		      `(,org-easy-blog-default-ext . 1) nil nil nil)))
  (org-easy-blog-with-env
   (let ((filename (expand-file-name post-file org-easy-blog-postdir))
	 (file-ext (file-name-extension post-file)))
     (when (not (member file-ext org-easy-blog--formats))
       (error "Please enter .org file name"))
     (when (file-exists-p (file-truename filename))
       (error "%s already exists!" filename))

     (when (get-buffer "*blog*")
       (kill-buffer "*blog*"))
     (find-file filename)
     (when (string-equal file-ext "org") ;; quand c'est un fichier org
       (insert (org-easy-blog--org-headers (file-name-base post-file)))) ;; inserer une entete definis plus haut
     (goto-char (point-max))
     (save-buffer))))

;;;###autoload
(defun org-easy-blog ()
  "Easy blog mode."
  (interactive)
  (org-easy-blog-with-env

	 (let ((content-dir (concat org-easy-blog-basedir org-easy-blog-postdir)))
		 (make-directory content-dir :parents))
	 
	 ;; (unless (file-directory-p (expand-file-name org-easy-blog-postdir org-easy-blog-basedir))
   ;;   (error "%s%s does not exist!" org-easy-blog-basedir org-easy-blog-postdir))
	 
   (setq org-easy-blog--mode-buffer (get-buffer-create org-easy-blog--buffer-name))
   (setq org-easy-blog--draft-list nil)
   (switch-to-buffer org-easy-blog--mode-buffer)
   (setq-local default-directory org-easy-blog-basedir)
   (setq buffer-read-only nil)
   (erase-buffer)
   (if (equal (file-relative-name org-easy-blog-postdir "content") ".")
       (insert (propertize
								(concat "org-easy-blog  " org-easy-blog-url "/" "\n\n")
								'face
								'org-easy-blog-help-face))
     (insert (propertize
							(concat "org-easy-blog  " org-easy-blog-url "/"
											(file-relative-name org-easy-blog-postdir "content")
											"\n\n")
							'face
							'org-easy-blog-help-face)))
   (unless org-easy-blog-no-help
     (insert (propertize org-easy-blog-help 'face 'org-easy-blog-help-face))
     (when org-easy-blog-additional-help
       (insert (propertize org-easy-blog-add-help 'face 'org-easy-blog-help-face)))
     (insert (propertize (concat "\n")'face 'org-easy-blog-help-face)))
   (unless org-easy-blog--refresh
     (setq org-easy-blog--cursor (point)))
   (let ((files (directory-files (expand-file-name org-easy-blog-postdir org-easy-blog-basedir)))
				 (lists (list)))
     (if (eq 2 (length files))
				 (progn
					 (insert org-easy-blog--first-help)
					 (org-easy-blog-mode)
					 (goto-char org-easy-blog--cursor))
       (progn
				 (cond ((eq 1 org-easy-blog--sort-char-flg)
								(setq files (reverse (sort files 'string<))))
							 ((eq 2 org-easy-blog--sort-char-flg)
								(setq files (sort files 'string<)))
							 ((eq 1 org-easy-blog--sort-publishday-flg)
								(let ((publist (org-easy-blog--publishday-alist)))
									(if publist
											(let ((source (sort publist
																					(lambda (a b) (string> (car a) (car b))))))
												(setq files nil)
												(while source
													(push (file-relative-name (cdr (car source))
																										(expand-file-name org-easy-blog-postdir org-easy-blog-basedir))
																files)
													(pop source)))
										(message "There is no file written date in front matter"))))
							 ((eq 2 org-easy-blog--sort-publishday-flg)
								(let ((publist (org-easy-blog--publishday-alist)))
									(if publist
											(let ((source (reverse (sort publist
																									 (lambda (a b) (string> (car a) (car b)))))))
												(setq files nil)
												(while source
													(push (file-relative-name (cdr (car source))
																										(expand-file-name org-easy-blog-postdir org-easy-blog-basedir))
																files)
													(pop source)))
										(message "There is no file written date in front matter")))))
				 (while files
					 (unless (or (string= (car files) ".")
											 (string= (car files) "..")
											 (not (member (file-name-extension (car files)) org-easy-blog--formats)))
						 (push
							(concat
							 (format-time-string "%Y-%m-%d %H:%M:%S "
																	 (nth 5 (file-attributes
																					 (expand-file-name
																						(car files)
																						org-easy-blog-postdir))))
							 (car files))
							lists))
					 (pop files))
				 (cond ((eq 1 org-easy-blog--sort-time-flg)
								(setq lists (reverse (sort lists 'string<))))
							 ((eq 2 org-easy-blog--sort-time-flg)
								(setq lists (sort lists 'string<))))
				 (while lists
					 (insert (concat (car lists) "\n"))
					 (pop lists))
				 (goto-char org-easy-blog--cursor)
				 (org-easy-blog-ignore-error
             (if org-easy-blog--refresh
								 (progn
									 (when (< (line-number-at-pos) org-easy-blog--unmovable-line)
										 (goto-char (point-min))
										 (forward-line (- org-easy-blog--unmovable-line 1)))
									 (beginning-of-line)
									 (forward-char org-easy-blog--forward-char))
							 (forward-char org-easy-blog--forward-char)))
				 (org-easy-blog-mode)
				 (when org-easy-blog-emacspeak
					 (org-easy-blog-emacspeak-filename)))))))

(provide 'org-easy-blog)
;;; org-easy-blog.el ends here
