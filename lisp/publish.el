;;; publish.el --- prepares websites for publishing -*- lexical-binding: t; -*-

;; Author: Aziz Ben Ali <tahaaziz.benali@esprit.tn>
;; URL: https://github.com/greppers/docs

;; publish.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; publish.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with publish.el.  If not, see <https://www.gnu.org/licenses/>.

;; This file contains code taken from GNU Emacs, which is
;; Copyright (C) 1976-2022 Free Software Foundation, Inc.

;;; Commentary:

;; publish.el specifies the structure of a website built atop
;; org-publish-project-alist.  It should also specify any other
;; options that affect the building of the website.

(normal-top-level-add-subdirs-to-load-path)

(require 'ox-publish)
(require 'project)
(require 'forgecast)

(setq default-directory
      (expand-file-name
       (project-root (project-current))))

;;; Configurations:

;; Org caching:

(setq org-export-time-stamp-file nil
      org-publish-timestamp-directory ".cache/")

;; Org content metadata:

(setq org-html-metadata-timestamp-format "%B %d, %Y")

;; Org source blocks:

(setq org-src-fontify-natively t
      org-src-preserve-indentation t
      org-html-htmlize-output-type nil)

;; Emacs:

(setq make-backup-files nil)

;; Metadata:

(setq user-full-name "greppers"
      user-mail-address "tahaaziz.benali@esprit.tn")

;; Functions:

(defun org-html-format-spec (info)
  "Return format specification for preamble and postamble.
INFO is a plist used as a communication channel."
  (let ((timestamp-format (plist-get info :html-metadata-timestamp-format)))
    `((?e . ,(forgecast-get-resource-url 'edit))
      (?t . ,(forgecast-get-resource-url 'tree))
      (?C . ,(let ((file (plist-get info :input-file)))
	       (format-time-string timestamp-format
				   (and file (file-attribute-modification-time
					      (file-attributes file)))))))))

(defun publish-read-template (file)
  "Read a file from the templates directory."
  (with-temp-buffer
    (insert-file-contents
     (file-name-concat "src" "templates" file))
    (buffer-string)))

(defun publish-html-head ()
  (string-join
   '("<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" />"
     "<link rel=\"stylesheet\" href=\"/css/main.css\" />"
     "<link rel=\"icon\" type=\"image/x-icon\" href=\"https://avatars.githubusercontent.com/u/117290777\" />")
   "\n"))

;;; Project specification:

(setq org-publish-project-alist
      (let ((main-preamble (publish-read-template "preamble/content.html"))
	    (docs-postamble (publish-read-template "postamble/docs.html"))
	    (html-head (publish-html-head)))
	(list
	 (list "main"
	       :base-extension "org"
	       :base-directory "src"
	       :publishing-directory "public"
	       :publishing-function 'org-html-publish-to-html
	       :section-numbers t
	       :with-toc t
	       :with-title t
	       :html-doctype "html5"
	       :html-html5-fancy t
	       :html-preamble main-preamble
	       :html-postamble nil
	       :html-head-extra html-head
	       :html-head-include-default-style nil)
	 (list "assets"
	       :base-extension (regexp-opt '("png" "svg" "jpe?g"))
	       :base-directory "src/assets"
	       :publishing-directory "public/assets"
	       :publishing-function 'org-publish-attachment
	       :recursive t)
	 (list "docs"
	       :base-extension "org"
	       :base-directory "src/docs"
	       :publishing-directory "public/docs"
	       :publishing-function 'org-html-publish-to-html
	       :makeindex t
	       :section-numbers t
	       :with-toc t
	       :with-title t
	       :html-doctype "html5"
	       :html-html5-fancy t
	       :html-preamble main-preamble
	       :html-postamble docs-postamble
	       :html-head-extra html-head
	       :html-head-include-default-style nil)
	 (list "css"
	       :base-extension "css"
	       :base-directory "src/css"
	       :publishing-directory "public/css"
	       :publishing-function 'org-publish-attachment)
	 (list "all"
	       :components (list "main" "docs" "assets" "css")))))
