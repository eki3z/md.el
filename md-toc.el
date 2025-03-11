;;; md-toc.el --- TOC management for md-ts-mode -*- lexical-binding: t -*-

;; Copyright (C) 2025 Eki Zhang

;; Author: Eki Zhang <liuyinz95@gmail.com>
;; Homepage: https://github.com/eki3z/md

;; This file is not a part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is not a part of GNU Emacs.

;;; Commentary:

;; This package aims to generate and manage TOC automatically for `md-ts--mode'

;;; Code:

(require 'seq)
(require 'pcase)

(require 'md-ts-mode)
(declare-function treesit-node-type "treesit.c")

(defcustom md-toc-item-bullet "-"
  "Bullet marker used in the Markdown table of contents.
Determines the unordered list style for TOC entries."
  :type '(choice
          (const :tag "Hyphen" "-")
          (const :tag "Asterisk" "*")
          (const :tag "Plus" "+"))
  :group 'md)

(defcustom md-toc-template
  '("<!-- md-toc start - Don't edit this section. Run M-x md-toc-update-doc -->"
    "**Table of Contents**"
    "<!-- md-toc end -->")
  "Template for md toc, consisting of start delimiter, title, and end delimiter.
Used to frame the TOC when inserted or updated via `md-toc-update-doc'."
  :type '(list (string :tag "Start Delimiter")
               (string :tag "Title")
               (string :tag "End Delimiter"))
  :group 'md)


(defvar-local md-toc--cache nil
  "Cons cell of (ENTRIES . BUFFER-HASH).
ENTRIES is a list of TOC entries for Markdown documents.
Each entry is a list of (INDENT-LEVEL TITLE LINK MARKER), where:
- LINK is the anchor link (string),
- MARKER is the buffer position of the entry (marker),
- TITLE is the entry text (string),
- INDENT-LEVEL is the nesting level (integer).")

(defconst md-toc--transform-rx
  (rx (or (group space)
          (group punctuation)))
  "Precompiled regex for link transformation.
- replace all spaces with dashes,
- remove all punctuation except - and _.")

(defun md-toc--uniquify-link (entries)
  "Return ENTRIES which have uniq links."
  (let ((seen-links (make-hash-table :test #'equal)))
    (mapcar
     (pcase-lambda (`(,link . ,rest))
       (let* ((count (gethash link seen-links 0))
              (new-link (if (zerop count) link (format "%s-%d" link count))))
         (puthash link (1+ count) seen-links)
         (cons new-link rest)))
     entries)))

(defun md-toc--range ()
  "Return markdown toc position as (START . END) if exists, else nil.
Only return the first pair of TOC."
  (pcase-let* ((`(,start ,_ ,end) md-toc-template))
    (when-let* (((derived-mode-p 'md-ts-mode))
                (html-root (treesit-buffer-root-node 'html))
                (comments (treesit-filter-child
                           html-root
                           (lambda(x)
                             (string= (treesit-node-type x) "comment")) t))
                (toc-start (seq-find
                            (lambda (x) (string= (treesit-node-text x) start))
                            comments))
                (toc-end (seq-find
                          (lambda (x) (string= (treesit-node-text x) end))
                          comments)))
      (cons (treesit-node-start toc-start)
            (treesit-node-end toc-end)))))

(defun md-toc--enable-read-only (&optional disable)
  "Make markdown TOC part read only.
If optional arg DISABLE is non-nil, disable it."
  (let ((modified (buffer-modified-p)))
    (when-let* ((region (md-toc--range))
                (start (car region))
                (end (cdr region)))
      (if disable
          (remove-text-properties start end '(read-only))
        (put-text-property start end 'read-only t)))
    (set-buffer-modified-p modified)))

(defun md-toc--cache-latest-p ()
  "Return non-nil if entries is latest."
  (and md-toc--cache
       (equal (cdr md-toc--cache) (buffer-chars-modified-tick))))

(defun md-toc--generate ()
  "Generate the Markdown TOC."
  ;; update md-toc--cache if needed
  (unless (md-toc--cache-latest-p)
    (thread-last
      (funcall imenu-create-index-function)
      (seq-mapcat
       (pcase-lambda (`(,level . ,headers))
         (mapcar
          (pcase-lambda (`(,title . ,marker))
            (list (marker-position marker)
                  (replace-regexp-in-string
                   md-toc--transform-rx
                   (lambda (match)
                     ;; replace space with dash, and remove all punctuation except
                     ;; dash and underline
                     (cond ((match-string 1 match) "-")
                           ((string-match-p "[-_]" match) match)
                           (t "")))
                   (downcase (string-trim title)))
                  marker
                  (substring-no-properties title)
                  (string-to-number (substring level 1))))
          headers)))
      (seq-sort-by #'car #'<)
      (mapcar #'cdr)
      (md-toc--uniquify-link)
      (list)
      (setq md-toc--cache)))

  ;; generate whole content
  (pcase-let ((`(,start ,head ,end) md-toc-template))
    (mapconcat #'identity
               (list start head
                     (mapconcat
                      (pcase-lambda (`(,link ,_ ,title ,level))
                        (format "%s%s [%s](#%s)"
                                (make-string (* (1- level)
                                                md-ts-mode-indent-offset) ?\s)
                                md-toc-item-bullet
                                title
                                link))
                      (car md-toc--cache)
                      "\n")
                     end)
               "\n\n")))

;;;###autoload
(defun md-toc-insert ()
  "Insert markdown TOC if not exists."
  (interactive nil md-ts-mode)
  (if (md-toc--range)
      (message "md-toc: TOC already exists.")
    (goto-char (pos-bol))
    (insert (md-toc--generate))
    (md-toc--enable-read-only)
    (setcdr md-toc--cache (buffer-chars-modified-tick))))

;;;###autoload
(defun md-toc-delete ()
  "Delete the TOC of current buffer if find."
  (interactive nil md-ts-mode)
  (if-let* ((region (md-toc--range))
            (inhibit-read-only t))
      (delete-region (car region) (cdr region))
    (message "md-toc: TOC does not exist.")))

;;;###autoload
(defun md-toc-update ()
  "Update markdown TOC if exists."
  (interactive nil md-ts-mode)
  (if-let* ((region (md-toc--range))
            ((not (md-toc--cache-latest-p))))
      (save-excursion
        (md-toc-delete)
        (goto-char (car region))
        (md-toc-insert))
    (message "md-toc: TOC does not exist or is latest already.")))

;;;###autoload
(defun md-toc-follow-link-at-point ()
  "On a TOC link, navigate to the related markdown header."
  (interactive nil md-ts-mode)
  (when-let* ((region (md-toc--range))
              ;; inside TOC region
              ((> (point) (car region)))
              ((< (point) (cdr region)))
              ;; detect link_destination node
              (node (treesit-node-at (- (pos-eol) 2) 'markdown-inline t))
              ((string= (treesit-node-type node) "link_destination"))
              (link (substring-no-properties (treesit-node-text node) 1))
              (marker (cadr (assoc link (car md-toc--cache)))))
    (goto-char marker)))

;;;###autoload
(define-minor-mode md-toc-mode
  "Minor mode to auto update markdown TOC in `md-ts-mode'."
  :global nil
  :lighter " mt"
  :group 'md
  (if md-toc-mode
      (progn
        (add-hook 'before-save-hook #'md-toc-update t)
        (md-toc--enable-read-only))
    (remove-hook 'before-save-hook #'md-toc-update t)
    (md-toc--enable-read-only 'disable)))

(provide 'md-toc)
;;; md-toc.el ends here
