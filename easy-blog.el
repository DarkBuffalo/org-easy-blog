;;; easy-blog.el --- Write blog easy -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Matthias David
;;
;; Author: Matthias David <https://github.com/DarkBuffalo>
;; Maintainer: Matthias David <darkbuffalo@delta.re>
;; Created: mars 26, 2021
;; Modified: mars 26, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/DarkBuffalo/easy-blog
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

(defcustom easy-blog-url nil
  "Url of the site."
  :group 'easy-blog
  :type 'string)

(defcustom easy-blog-basedir nil
  "Directory where html source code is placed."
  :group 'easy-blog
  :type 'string)

(defcustom easy-blog-postdir "content/post"
  "Directory where the theme stores its posts."
  :group 'easy-blog
  :type 'string)

(defcustom easy-blog-sort-default-char nil
  "Default setting to sort with charactor."
  :group 'easy-blog
  :type 'integer)

(defcustom easy-blog-no-help nil
  "No help flg of `easy-blog'."
  :group 'easy-blog
  :type 'integer)

(defcustom easy-blog-help-line 7
  "Number of lines of `easy-blog-help'."
  :group 'easy-blog
  :type 'integer)

(defcustom easy-blog-additional-help nil
  "Additional help flg of `easy-blog'."
  :group 'easy-blog
  :type 'integer)

(defconst easy-blog--buffer-name "*Easy-blog*"
  "Buffer name of `easy-blog'.")

(defconst easy-blog--formats `("org"))

(defconst easy-blog--forward-char 20
  "Forward-char of `easy-blog'.")

(defface easy-blog-help-face
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
  :group 'easy-blog-faces)

(defvar easy-blog--mode-buffer nil
  "Main buffer of `easy-blog'.")

(defvar easy-blog--cursor nil
  "Cursor of `easy-blog'.")

(defvar easy-blog--refresh nil
  "Refresh flg of `easy-blog'.")

(defvar easy-blog--draft-list nil
  "Draft list flg.")

(defvar easy-blog--sort-char-flg nil
  "Sort char flg of `easy-blog'.")

(defvar easy-blog--sort-publishday-flg nil
  "Sort publishtime flg of `easy-blog'.")

(defvar easy-blog--sort-time-flg 1
  "Sort time flg of `easy-blog'.")

(if easy-blog-no-help
    (defvar easy-blog--unmovable-line 3
      "Impossible to move below this line.")
  (defvar easy-blog--unmovable-line (+ easy-blog-help-line 4)
    "Impossible to move below this line."))

(defmacro easy-blog-with-env (&rest body)
  "Evaluate BODY with `default-directory' set to `easy-blog-basedir'.
Report an error if `easy-blog-basedir' is unset."
  `(progn
     (unless easy-blog-basedir
       (error "Please set easy-blog-basedir variable"))
     (let ((default-directory easy-blog-basedir))
       ,@body)))

(defmacro easy-blog-ignore-error (condition &rest body)
  "Execute BODY; if the error CONDITION occurs, return nil.
Otherwise, return result of last form in BODY.
CONDITION can also be a list of error conditions."
  (declare (debug t) (indent 1))
  `(condition-case nil (progn ,@body) (,condition nil)))

(defcustom easy-blog-emacspeak nil
  "Emacspeak flg of `easy-blog'."
  :group 'easy-blog
  :type 'integer)

(defcustom easy-blog-default-ext ".org"
  "Default extension when posting new articles."
  :group 'easy-blog
  :type 'string)

(defcustom easy-blog-bloglist nil
  "Multiple blog setting."
  :group 'easy-blog
  :type 'string)

(push `((easy-blog-basedir . ,easy-blog-basedir)
	(easy-blog-url . ,easy-blog-url)
	;;(easy-blog-root . ,easy-blog-root)
	;;(easy-blog-sshdomain . ,easy-blog-sshdomain)
	(easy-blog-postdir . ,easy-blog-postdir)
	(easy-blog-sort-default-char . ,easy-blog-sort-default-char)
	(easy-blog-default-ext . ,easy-blog-default-ext))
      easy-blog-bloglist)


(defcustom easy-blog-help
  (if (null easy-blog-sort-default-char)
      (progn
	"n .. New blog post    R .. Rename file    D .. Draft list
p .. Preview          g .. Refresh       u .. Sort publishday
v .. Open view-mode   s .. Sort time     T .. Publish timer    N .. No help-mode
d .. Delete post      c .. Open config   W .. AWS S3 timer     f .. Select filename
P .. Publish clever   C .. Deploy GCS    a .. Search with ag   H .. GitHub timer
< .. Previous blog    > .. Next blog     , .. Pre postdir      . .. Next postdir
/ .. Select postdir   q .. Quit easy-hugo
")
    (progn
      "n .. New blog post    R .. Rename file    D .. Draft list
p .. Preview          g .. Refresh       u .. Sort publishday
v .. Open view-mode   s .. Sort char     T .. Publish timer    N .. No help-mode
d .. Delete post      c .. Open config   ; .. Select blog      f .. Select filename
P .. Publish clever   C .. Deploy GCS    a .. Search with ag   H .. GitHub timer
< .. Previous blog    > .. Next blog     , .. Pre postdir      . .. Next postdir
/ .. Select postdir   q .. Quit easy-hugo
"))
  "Help of `easy-blog'."
  :group 'easy-blog
  :type 'string)


(defconst easy-blog--first-help
  "Welcome to Easy-blog
Let's post an article first.
Press n on this screen or M-x easy-hugo-newpost.
Enter a article file name in the minibuffer.
Then M-x easy-hugo again or refresh the screen with r or g key in this buffer,
article which you wrote should appear here.
Enjoy!
"
  "Help of `easy-blog' first time.")

(defcustom easy-blog-add-help
  (if (null easy-blog-sort-default-char)
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
  "Add help of `easy-blog'."
  :group 'easy-blog
  :type 'string)

(defvar easy-blog-mode-map
  (let ((map (make-keymap)))
    (define-key map "n" 'easy-blog-newpost)
    (define-key map "q" 'easy-blog-quit)
    (define-key map [backtab] 'easy-blog-no-help)
    (define-key map "r" 'easy-hugo-refresh)

    map)
  "Keymap for `easy-blog' major mode.")

(defun easy-blog-quit ()
  "Quit easy blog."
  (interactive)
  (setq easy-blog--sort-time-flg 1)
  (setq easy-blog--sort-char-flg nil)
  (easy-hugo--preview-end)
  (when (buffer-live-p easy-blog--mode-buffer)
    (kill-buffer easy-blog--mode-buffer)))


(define-derived-mode easy-blog-mode special-mode "Easy-blog"
  "Major mode for easy hugo.")

(defsubst easy-blog--directory-name-p (name)
  "Return non-nil if NAME ends with a directory separator character."
  (let ((len (length name))
        (lastc ?.))
    (if (> len 0)
        (setq lastc (aref name (1- len))))
    (or (= lastc ?/)
        (and (memq system-type '(windows-nt ms-dos))
             (= lastc ?\\)))))

(defun easy-blog-emacspeak-filename ()
  "Read filename with emacspeak."
  (cl-declare (special emacspeak-speak-last-spoken-word-position))
  (let ((filename (substring (thing-at-point 'line) easy-blog--forward-char -1))
        (personality (dtk-get-style)))
    (cond
     (filename
      (dtk-speak (propertize filename 'personality personality))
      (setq emacspeak-speak-last-spoken-word-position (point)))
     (t (emacspeak-speak-line)))))

(defun easy-blog--directory-files (dir regexp)
  "Return list of all files under DIR that have file names matching REGEXP."
  (let ((result nil)
	(files nil)
	(tramp-mode (and tramp-mode (file-remote-p (expand-file-name dir)))))
    (dolist (file (sort (file-name-all-completions "" dir)
			'string<))
      (unless (member file '("./" "../"))
	(if (not (easy-blog--directory-name-p file))
	    (when (string-match regexp file)
	      (push (expand-file-name file dir) files)))))
    (nconc result (nreverse files))))

(defun easy-blog--publishday-alist ()
  "Return article alist with publishing date."
  (let* ((files (easy-blog--directory-files
		 (expand-file-name
		  easy-blog-postdir
		  easy-blog-basedir)
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

(defun easy-blog--orgtime-format (x)
  "Format orgtime as X."
  (concat (substring x 0 3) ":" (substring x 3 5)))


(defun easy-blog--org-headers (file)
  "Return a draft org mode header string for a new article as FILE."
  (let ((datetimezone
         (concat
          (format-time-string "%Y-%m-%dT%T")
          (easy-blog--orgtime-format (format-time-string "%z")))))
    (concat
     "#+TITLE: " file
     "\n#+DATE: " datetimezone
     "\n#+PUBLISHDATE: " datetimezone
     "\n#+DRAFT: nil"
     "\n#+TAGS: nil, nil"
     "\n#+DESCRIPTION: Short description"
     "\n\n")))

;;;###autoload
(defun easy-blog-newpost (post-file)
  "Create a new post with hugo.
POST-FILE needs to have and extension  '.org'."
  (interactive (list (read-from-minibuffer
		      "Filename: "
		      `(,easy-blog-default-ext . 1) nil nil nil)))
  (easy-blog-with-env
   (let ((filename (expand-file-name post-file easy-blog-postdir))
	 (file-ext (file-name-extension post-file)))
     (when (not (member file-ext easy-blog--formats))
       (error "Please enter .org file name"))
     (when (file-exists-p (file-truename filename))
       (error "%s already exists!" filename))

     (when (get-buffer "*blog*")
       (kill-buffer "*blog*"))
     (find-file filename)
     (when (string-equal file-ext "org") ;; quand c'est un fichier org
       (insert (easy-blog--org-headers (file-name-base post-file)))) ;; inserer une entete definis plus haut
     (goto-char (point-max))
     (save-buffer))))

;;;###autoload
(defun easy-blog ()
  "Easy blog mode."
  (interactive)
  (easy-blog-with-env
   (unless (file-directory-p (expand-file-name easy-blog-postdir easy-blog-basedir))
     (error "%s%s does not exist!" easy-blog-basedir easy-blog-postdir))
   (setq easy-blog--mode-buffer (get-buffer-create easy-blog--buffer-name))
   (setq easy-blog--draft-list nil)
   (switch-to-buffer easy-blog--mode-buffer)
   (setq-local default-directory easy-blog-basedir)
   (setq buffer-read-only nil)
   (erase-buffer)
   (if (equal (file-relative-name easy-blog-postdir "content") ".")
       (insert (propertize
		(concat "Easy-blog  " easy-blog-url "/" "\n\n")
		'face
		'easy-blog-help-face))
     (insert (propertize
	      (concat "Easy-blog  " easy-blog-url "/"
		      (file-relative-name easy-blog-postdir "content")
		      "\n\n")
	      'face
	      'easy-blog-help-face)))
   (unless easy-blog-no-help
     (insert (propertize easy-blog-help 'face 'easy-blog-help-face))
     (when easy-blog-additional-help
       (insert (propertize easy-blog-add-help 'face 'easy-blog-help-face)))
     (insert (propertize (concat "\n")'face 'easy-blog-help-face)))
   (unless easy-blog--refresh
     (setq easy-blog--cursor (point)))
   (let ((files (directory-files (expand-file-name easy-blog-postdir easy-blog-basedir)))
	 (lists (list)))
     (if (eq 2 (length files))
	 (progn
	   (insert easy-blog--first-help)
	   (easy-blog-mode)
	   (goto-char easy-blog--cursor))
       (progn
	 (cond ((eq 1 easy-blog--sort-char-flg)
		(setq files (reverse (sort files 'string<))))
	       ((eq 2 easy-blog--sort-char-flg)
		(setq files (sort files 'string<)))
	       ((eq 1 easy-blog--sort-publishday-flg)
		(let ((publist (easy-blog--publishday-alist)))
		  (if publist
		      (let ((source (sort publist
					  (lambda (a b) (string> (car a) (car b))))))
			(setq files nil)
			(while source
			  (push (file-relative-name (cdr (car source))
						    (expand-file-name easy-blog-postdir easy-blog-basedir))
				files)
			  (pop source)))
		    (message "There is no file written date in front matter"))))
	       ((eq 2 easy-blog--sort-publishday-flg)
		(let ((publist (easy-blog--publishday-alist)))
		  (if publist
		      (let ((source (reverse (sort publist
						   (lambda (a b) (string> (car a) (car b)))))))
			(setq files nil)
			(while source
			  (push (file-relative-name (cdr (car source))
						    (expand-file-name easy-blog-postdir easy-blog-basedir))
				files)
			  (pop source)))
		    (message "There is no file written date in front matter")))))
	 (while files
	   (unless (or (string= (car files) ".")
		       (string= (car files) "..")
		       (not (member (file-name-extension (car files)) easy-blog--formats)))
	     (push
	      (concat
	       (format-time-string "%Y-%m-%d %H:%M:%S "
				   (nth 5 (file-attributes
					   (expand-file-name
					    (car files)
					    easy-blog-postdir))))
	       (car files))
	      lists))
	   (pop files))
	 (cond ((eq 1 easy-blog--sort-time-flg)
		(setq lists (reverse (sort lists 'string<))))
	       ((eq 2 easy-blog--sort-time-flg)
		(setq lists (sort lists 'string<))))
	 (while lists
	   (insert (concat (car lists) "\n"))
	   (pop lists))
	 (goto-char easy-blog--cursor)
	 (easy-blog-ignore-error
             (if easy-blog--refresh
		 (progn
	           (when (< (line-number-at-pos) easy-blog--unmovable-line)
		     (goto-char (point-min))
		     (forward-line (- easy-blog--unmovable-line 1)))
	           (beginning-of-line)
	           (forward-char easy-blog--forward-char))
	       (forward-char easy-blog--forward-char)))
	 (easy-blog-mode)
	 (when easy-blog-emacspeak
	   (easy-blog-emacspeak-filename)))))))

(provide 'easy-blog)
;;; easy-blog.el ends here
