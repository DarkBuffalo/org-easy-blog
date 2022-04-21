;;; org-easy-blog.el --- Write blog easy -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Matthias David

;; Author: Matthias David <darkbuffalo@gnu.re>
;; Keywords: comm, news
;; Homepage: https://github.com/DarkBuffalo/org-easy-blog
;; Package-Requires: ((emacs "24.3") (org) (simple-httpd "1.4.0")

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>

;;; Commentary:
;;
;;  Write blog easily with org
;;
;;; Code:

(require 'cl-lib)
(require 'org)
(require 'ox-html)
(require 'ox-publish)
(require 'simple-httpd)

(defcustom org-easy-blog-url nil
  "Url of the site."
  :group 'org-easy-blog
  :type 'string)

(defcustom org-easy-blog-basedir nil
  "Directory where html source code is placed."
  :group 'org-easy-blog
  :type 'string)

(defcustom org-easy-blog-root nil
  "Root directory of blog at your server."
  :group 'org-easy-blog
  :type 'string)

(defcustom org-easy-blog-sshdomain nil
  "Domain of blog at your ~/.ssh/config."
  :group 'prg-easy-blog
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

(defconst org-easy-blog--preview-buffer "*org-easy-blog Preview*"
  "Org-easy-blog preview buffer name.")

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


(defvar org-easy-blog--current-postdir 0
  "Easy-hugo current postdir.")

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

(defcustom org-easy-blog-image-directory "images"
  "Image file directory under 'static' directory."
  :group 'org-easy-blog
  :type 'string)

(defconst org-easy-blog--default-image-directory
  "images"
  "Default `org-easy-blog' image-directory.")


(defvar org-easy-blog--current-blog 0
  "Current blog number.")

(defcustom org-easy-blog-bloglist nil
  "Multiple blog setting."
  :group 'org-easy-blog
  :type 'string)



(push `((org-easy-blog-basedir . ,org-easy-blog-basedir)
				(org-easy-blog-url . ,org-easy-blog-url)
				(org-easy-blog-root . ,org-easy-blog-root)
				(org-easy-blog-sshdomain . ,org-easy-blog-sshdomain)
				(org-easy-blog-postdir . ,org-easy-blog-postdir)
				(org-easy-blog-image-directory . ,org-easy-blog-image-directory)
				(org-easy-blog-sort-default-char . ,org-easy-blog-sort-default-char)
				(org-easy-blog-default-ext . ,org-easy-blog-default-ext))
      org-easy-blog-bloglist)

;; SERVE
(defvar org-easy-blog--server-process nil
  "Server process.")

(defvar org-easy-blog--local-port
  3000
  "Port to serve local blog instance.")


;;;###autoload
(defun org-easy-blog-rg ()
  "Search for blog article with `counsel-rg' or `consult-ripgrep'."
  (interactive)
  (org-easy-blog-with-env
   (let ((dir (expand-file-name org-easy-blog-postdir org-easy-blog-basedir)))
     (if (require 'counsel nil t)
         (counsel-rg nil dir)
       (if (require 'consult nil t)
           (consult-ripgrep dir nil)
         (error "'counsel' or 'consult' is not installed"))))))


(defcustom org-easy-blog-help
  (if (null org-easy-blog-sort-default-char)
      (progn
				"n .. New blog post    e .. Edit file    R .. Rename file      D .. Draft list
p .. Preview          g .. Refresh          u .. Sort publishday
v .. Open view-mode   s .. Sort time        T .. Publish timer     N .. No help-mode
d .. Delete post      f .. Select filename
P .. Publish clever   a .. Search with rg
< .. Previous blog    > .. Next blog        , .. Pre postdir       . .. Next postdir
/ .. Select postdir   q .. Quit easy-blog
")
    (progn
      "n .. New blog post    e .. Edit file     R .. Rename file      D .. Draft list
p .. Preview          g .. Refresh          u .. Sort publishday
v .. Open view-mode   s .. Sort char        T .. Publish timer     N .. No help-mode
d .. Delete post      ; .. Select blog      f .. Select filename
P .. Publish clever   a .. Search with rg
< .. Previous blog    > .. Next blog        , .. Pre postdir       . .. Next postdir
/ .. Select postdir   q .. Quit easy-blog
"))
  "Help of `org-easy-blog'."
  :group 'org-easy-blog
  :type 'string)


(defconst org-easy-blog--first-help
  "Welcome to org-easy-blog
Let's post an article first.
Press n on this screen or M-x org-easy-blog-newpost.
Enter a article file name in the minibuffer.
Then M-x easy-hugo again or refresh the screen with r or g key in this buffer,
article which you wrote should appear here.
Enjoy!
"
  "Help of `org-easy-blog' first time.")

(defcustom org-easy-blog-add-help
  (if (null org-easy-blog-sort-default-char)
      (progn
	"O .. Open basedir     r .. Refresh       t .. X publish-timer
k .. Previous-line    j .. Next line     h .. backward-char    l .. forward-char
V .. View other window
- .. Pre postdir      + .. Next postdir  w .. Write post       o .. Open other window
J .. Jump blog        e .. Edit file
S .. Sort char     M .. Magit status     ? .. Describe-mode
")
    (progn
      "O .. Open basedir     r .. Refresh       t .. X publish-timer
k .. Previous-line    j .. Next line     h .. backward-char    l .. forward-char
V .. View other window
- .. Pre postdir      + .. Next postdir  w .. Write post       o .. Open other window
J .. Jump blog        e .. Edit file
S .. Sort time     M .. Magit status     ? .. Describe-mode
"))
  "Add help of `org-easy-blog'."
  :group 'org-easy-blog
  :type 'string)

(defvar org-easy-blog-mode-map
  (let ((map (make-keymap)))
    (define-key map "n" 'org-easy-blog-newpost)
    (define-key map "q" 'org-easy-blog-quit)
    (define-key map [backtab] 'org-easy-blog-no-help)
    (define-key map "r" 'org-easy-blog-refresh)
    (define-key map "p" 'org-easy-blog-preview)
    (define-key map "<" 'org-easy-blog-previous-blog)
    (define-key map ">" 'org-easy-blog-next-blog)
		(define-key map "a" 'org-easy-blog-rg)
		(define-key map "e" 'org-easy-blog-open)
    map)
  "Keymap for `org-easy-blog' major mode.")



(setf (alist-get "oeb-posts" org-publish-project-alist nil nil #'string=)
			(list
			 :base-directory (expand-file-name (concat org-easy-blog-basedir org-easy-blog-postdir))
			 :base-extension "org"
			 :recursive t
			 :publishing-function (list 'org-html-publish-to-html 'org-org-publish-to-org)
			 :publishing-directory (expand-file-name (concat org-easy-blog-basedir "www")) ;; local dir
			 :exclude (regexp-opt '("README.org" "rss.org" "index.org" ))
			 :exclude-tags (list "draft" "noexport")
			 :auto-sitemap t
			 :sitemap-filename "index.org"
			 :sitemap-title "Blog"
			 :sitemap-style 'list ;;tree
			 :sitemap-sort-files 'anti-chronologically
			 :with-latex t
			 :with-drawers t
			 :export-with-tags t
			 :section-numbers nil
			 :html-doctpe "html5"
			 :html-html5-fancy t
			 :html-head-include-scripts t
			 :html-head-include-default-style nil
			'("oeb-blog" :components ("oeb-posts"))))

(defun org-easy-blog-open ()
  "Open the file on the pointer."
  (interactive)
  (when (equal (buffer-name (current-buffer)) org-easy-blog--buffer-name)
    (org-easy-blog-with-env
     (unless (or (string-match "^$" (thing-at-point 'line))
								 (eq (point) (point-max))
								 (> (+ 1 org-easy-blog--forward-char) (length (thing-at-point 'line))))
       (let ((file (expand-file-name
										(substring (thing-at-point 'line) org-easy-blog--forward-char -1)
										org-easy-blog-postdir)))
				 (when (and (file-exists-p file)
										(not (file-directory-p file)))
					 (find-file file)))))))



(defun org-easy-blog-refresh ()
  "Refresh org-easy-blog-mode."
  (interactive)
  (setq org-easy-blog--cursor (point))
  (setq org-easy-blog--refresh 1)
  ;; (if org-easy-blog--draft-list
  ;;     (org-easy-blog-draft-list)
  ;;   (easy-hugo))
  (setq easy-hugo--refresh nil))


;;;###autoload
(defun org-easy-blog-preview ()
  "Preview at localhost."
  (interactive)
	(org-easy-blog-with-env
	 (org-publish "oeb-blog" t)
   (let ((www-dir (concat org-easy-blog-basedir "www"))) ;;change to public dir
		 ;; (make-directory www-dir :parents)
		 (setq httpd-root www-dir)
		 (setq httpd-port org-easy-blog--local-port)
		 (httpd-start)
		 (browse-url (format "http://localhost:%d"
												 org-easy-blog--local-port))
		 
     ;; (setq org-easy-blog--server-process
     ;;       (start-process "org-easy-blog--server-process" nil "python" "-m" "http.server" (format "%s" org-easy-blog--local-port)))
     ;; (browse-url (format "http://localhost:%s" org-easy-blog--local-port))
		 
		 )))

(defun org-easy-blog-preview-end ()
  "Finish previewing at localhost."
	(httpd-stop)
  (unless (httpd-running-p)
		(httpd-stop))
	
  (when (get-buffer org-easy-blog--preview-buffer)
		(kill-buffer org-easy-blog--preview-buffer)))




(defmacro org-easy-blog-set-bloglist (body)
  "Macros to set variables to `easy-hugo-bloglist' as BODY."
  `(setq ,body
	 (cdr (assoc ',body
		     (nth org-easy-blog--current-blog org-easy-blog-bloglist)))))

(defmacro org-easy-blog-eval-bloglist (body)
  "Macros to eval variables of BODY from `easy-hugo-bloglist'."
  `(cdr (assoc ',body
	       (nth org-easy-blog--current-blog org-easy-blog-bloglist))))

(defmacro org-easy-blog-nth-eval-bloglist (body blog)
  "Macros to eval variables of BODY from `easy-hugo-bloglist' at BLOG."
  `(cdr (assoc ',body
	       (nth ,blog org-easy-blog-bloglist))))



(defun org-easy-blog-next-blog ()
  "Go to next blog."
  (interactive)
  (when (< 1 (length org-easy-blog-bloglist))
    (if (eq (- (length org-easy-blog-bloglist) 1) org-easy-blog--current-blog)
				(setq org-easy-blog--current-blog 0)
      (setq org-easy-blog--current-blog (+ org-easy-blog--current-blog 1)))
    (setq org-easy-blog--current-postdir 0)
    (org-easy-blog-set-bloglist org-easy-blog-basedir)
    (org-easy-blog-set-bloglist org-easy-blog-url)
    (org-easy-blog-set-bloglist org-easy-blog-root)
    (org-easy-blog-set-bloglist org-easy-blog-sshdomain)

    (if (org-easy-blog-eval-bloglist org-easy-blog-image-directory)
				(org-easy-blog-set-bloglist org-easy-blog-image-directory)
      (setq org-easy-blog-image-directory org-easy-blog--default-image-directory))

		(if (org-easy-blog-eval-bloglist easy-hugo-default-picture-directory)
				(easy-hugo-set-bloglist easy-hugo-default-picture-directory)
      (setq easy-hugo-default-picture-directory easy-hugo--default-picture-directory))
    (if (org-easy-blog-eval-bloglist easy-hugo-preview-url)
				(easy-hugo-set-bloglist easy-hugo-preview-url)
      (setq easy-hugo-preview-url easy-hugo--default-preview-url))
    (if (org-easy-blog-eval-bloglist easy-hugo-publish-chmod)
				(easy-hugo-set-bloglist easy-hugo-publish-chmod)
      (setq easy-hugo-publish-chmod easy-hugo--default-publish-chmod))
    (if (org-easy-blog-eval-bloglist easy-hugo-previewtime)
				(easy-hugo-set-bloglist easy-hugo-previewtime)
      (setq easy-hugo-previewtime easy-hugo--default-previewtime))
    (if (org-easy-blog-eval-bloglist easy-hugo-sort-default-char)
				(easy-hugo-set-bloglist easy-hugo-sort-default-char)
      (setq easy-hugo-sort-default-char easy-hugo--default-sort-default-char))
    (if (org-easy-blog-eval-bloglist easy-hugo-asciidoc-extension)
				(easy-hugo-set-bloglist easy-hugo-asciidoc-extension)
      (setq easy-hugo-asciidoc-extension easy-hugo--default-asciidoc-extension))
    (if (org-easy-blog-eval-bloglist easy-hugo-html-extension)
				(easy-hugo-set-bloglist easy-hugo-html-extension)
      (setq easy-hugo-html-extension easy-hugo--default-html-extension))
    (if (org-easy-blog-eval-bloglist easy-hugo-markdown-extension)
				(easy-hugo-set-bloglist easy-hugo-markdown-extension)
      (setq easy-hugo-markdown-extension easy-hugo-markdown-extension))
    (if (org-easy-blog-eval-bloglist easy-hugo-default-ext)
				(easy-hugo-set-bloglist easy-hugo-default-ext)
      (setq easy-hugo-default-ext easy-hugo--default-ext))
    (if (org-easy-blog-eval-bloglist easy-hugo-postdir)
				(easy-hugo-set-bloglist easy-hugo-postdir)
      (setq easy-hugo-postdir easy-hugo--default-postdir))
    (org-easy-blog-preview-end)
    (easy-hugo)))

(defun org-easy-blog-previous-blog ()
  "Go to previous blog."
  (interactive)
  (when (< 1 (length org-easy-blog-bloglist))
    (if (= 0 org-easy-blog--current-blog)
	(setq org-easy-blog--current-blog (- (length org-easy-blog-bloglist) 1))
      (setq org-easy-blog--current-blog (- org-easy-blog--current-blog 1)))
    (setq org-easy-blog--current-postdir 0)
    (easy-hugo-set-bloglist org-easy-blog-basedir)
    (easy-hugo-set-bloglist org-easy-blog-url)
    (easy-hugo-set-bloglist org-easy-blog-root)
    (easy-hugo-set-bloglist org-easy-blog-sshdomain)

    (if (org-easy-blog-eval-bloglist org-easy-blog-image-directory)
				(easy-hugo-set-bloglist org-easy-blog-image-directory)
      (setq org-easy-blog-image-directory org-easy-blog--default-image-directory))
    (if (org-easy-blog-eval-bloglist easy-hugo-default-picture-directory)
				(easy-hugo-set-bloglist easy-hugo-default-picture-directory)
      (setq easy-hugo-default-picture-directory easy-hugo--default-picture-directory))
    (if (org-easy-blog-eval-bloglist easy-hugo-preview-url)
				(easy-hugo-set-bloglist easy-hugo-preview-url)
      (setq easy-hugo-preview-url easy-hugo--default-preview-url))
    (if (org-easy-blog-eval-bloglist easy-hugo-publish-chmod)
				(easy-hugo-set-bloglist easy-hugo-publish-chmod)
      (setq easy-hugo-publish-chmod easy-hugo--default-publish-chmod))
    (if (org-easy-blog-eval-bloglist easy-hugo-previewtime)
				(easy-hugo-set-bloglist easy-hugo-previewtime)
      (setq easy-hugo-previewtime easy-hugo--default-previewtime))
    (if (org-easy-blog-eval-bloglist easy-hugo-sort-default-char)
				(easy-hugo-set-bloglist easy-hugo-sort-default-char)
      (setq easy-hugo-sort-default-char easy-hugo--default-sort-default-char))
    (if (org-easy-blog-eval-bloglist easy-hugo-asciidoc-extension)
				(easy-hugo-set-bloglist easy-hugo-asciidoc-extension)
      (setq easy-hugo-asciidoc-extension easy-hugo--default-asciidoc-extension))
    (if (org-easy-blog-eval-bloglist easy-hugo-html-extension)
				(easy-hugo-set-bloglist easy-hugo-html-extension)
      (setq easy-hugo-html-extension easy-hugo--default-html-extension))
    (if (org-easy-blog-eval-bloglist easy-hugo-markdown-extension)
				(easy-hugo-set-bloglist easy-hugo-markdown-extension)
      (setq easy-hugo-markdown-extension easy-hugo-markdown-extension))
    (if (org-easy-blog-eval-bloglist easy-hugo-default-ext)
				(easy-hugo-set-bloglist easy-hugo-default-ext)
      (setq easy-hugo-default-ext easy-hugo--default-ext))
    (if (org-easy-blog-eval-bloglist easy-hugo-postdir)
				(easy-hugo-set-bloglist easy-hugo-postdir)
      (setq easy-hugo-postdir easy-hugo--default-postdir))
    (easy-hugo--preview-end)
    (easy-hugo)))














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
(defun easy-hugo-put-image ()
  "Move image to image directory and generate image link."
  (interactive
   (org-easy-blog-with-env
    (unless (file-directory-p (expand-file-name
                               org-easy-blog-image-directory
                               (expand-file-name "static" org-easy-blog-basedir)))
      (error "%s does not exist" (expand-file-name
                                  org-easy-blog-image-directory
                                  (expand-file-name "static" org-easy-blog-basedir))))
    (let ((insert-default-directory nil))
      (let* ((file (read-file-name "Image file: " nil
				   (expand-file-name org-easy-blog-default-picture-directory)
				   t
				   (expand-file-name org-easy-blog-default-picture-directory)))
	     (putfile (expand-file-name
		       (file-name-nondirectory file)
		       (expand-file-name org-easy-blog-image-directory "static"))))
	(when (file-exists-p putfile)
	  (error "%s already exists!" putfile))
	(copy-file file putfile)
	(insert (concat (format "{{< figure src=\"%s%s\""
				easy-hugo-url
				(concat
				 "/"
				 org-easy-blog-image-directory
				 "/"
				 (file-name-nondirectory file)))
			" alt=\"\" >}}")))))))

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









;;;###autoload
(defun org-easy-blog-publish-clever ()
  "Clever publish command.
Automatically select the deployment destination from init.el."
  (interactive)
  (org-easy-blog-with-env
   (cond ((org-easy-blog-eval-bloglist org-easy-blog-root)
					(org-easy-blog-publish))
				 (t (error "Nothing is found to publish at %s" org-easy-blog-basedir)))))

;;;###autoload
(defun org-easy-blog-publish ()
  "Adapt local change to the server with hugo."
  (interactive)
  (unless org-easy-blog-sshdomain
    (error "Please set org-easy-blog-sshdomain variable"))
  (unless org-easy-blog-root
    (error "Please set org-easy-blog-root variable"))
  (unless (file-exists-p "~/.ssh/config")
    (error "There is no ~/.ssh/config"))
	
  (
   (message "Blog published")
   (when org-easy-blog-url
     (browse-url org-easy-blog-url))))


(provide 'org-easy-blog)
;;; org-easy-blog.el ends here
