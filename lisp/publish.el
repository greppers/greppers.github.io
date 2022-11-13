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

(require 'ox-publish)
(require 'project)

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

(defun publish-read-template (file)
  "Read a file from the templates directory."
  (with-temp-buffer
    (insert-file-contents
     (file-name-concat "src" "templates" file))
    (buffer-string)))

(defun publish-html-head ()
  (string-join
   '("<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" />"
     "<link rel=\"icon\" type=\"image/x-icon\" href=\"https://avatars.githubusercontent.com/u/117290777\" />")
   "\n"))

;;; Project specification:

(setq org-publish-project-alist
      (let ((content-preamble (publish-read-template "preamble/content.html"))
	    (html-head (publish-html-head)))
	(list
	 (list "content"
	       :base-extension "org"
	       :base-directory "src"
	       :publishing-directory "public"
	       :publishing-function 'org-html-publish-to-html
	       :recursive t
	       :makeindex t
	       :section-numbers t
	       :with-toc t
	       :with-title t
	       :html-doctype "html5"
	       :html-html5-fancy t
	       :html-preamble content-preamble
	       :html-postamble nil
	       :html-head-extra html-head
	       :html-head-include-default-style nil)
	 (list "all"
	       :components (list "content")))))
