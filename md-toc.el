;;; md-toc.el --- summary -*- lexical-binding: t -*-

;; Author: Eki Zhang <liuyinz95@gmail.com>
;; Created: 2025-03-02 05:13:34

;;; Commentary:

;;; Code:

(require 'seq)
(require 'pcase)
(require 'md-ts-mode)

(defgroup md-toc nil
  "A simple TOC generator for markdown file."
  :group 'md)

(defcustom md-toc-item-bullet "-"
  "Bullet marker used in the Markdown table of contents.
Determines the unordered list style for TOC entries."
  :group 'md-toc
  :type '(choice
          (const :tag "Hyphen" "-")
          (const :tag "Asterisk" "*")
          (const :tag "Plus" "+")))

(defcustom md-toc-template
  '("<!-- md-toc start - Don't edit this section. Run M-x md-toc-update-doc -->"
    "**Table of Contents**"
    "<!-- md-toc end -->")
  "Template for md toc, consisting of start delimiter, title, and end delimiter.
Used to frame the TOC when inserted or updated via `md-toc-update-doc'."
  :group 'md-toc
  :type '(list (string :tag "Start Delimiter")
               (string :tag "Title")
               (string :tag "End Delimiter")))


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

(defun md-toc--region ()
  "Return markdown toc position as (BEGIN . END) if exists, else nil."
  (when (derived-mode-p 'md-ts-mode)
    (pcase-let* ((`(,start ,_ ,end) md-toc-template)
                 (rx-pattern (rx-to-string `(seq (group ,start)
                                                 (* anything)
                                                 (group ,end)) t))
                 (region (list nil)))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward rx-pattern nil t)
          (goto-char (match-beginning 1))
          (setcar region (pos-bol))
          (goto-char (match-end 2))
          (setcdr region (pos-eol))
          region)))))

(defun md-toc--cache-latest-p ()
  "Return non-nil if entries is latest."
  (and md-toc--cache
       (string= (cdr md-toc--cache) (buffer-hash))))

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
  (unless (md-toc--region)
    (goto-char (pos-bol))
    (insert (md-toc--generate))
    (setcdr md-toc--cache (buffer-hash))))

;;;###autoload
(defun md-toc-delete ()
  "Delete the TOC of current buffer if find."
  (interactive nil md-ts-mode)
  (when-let* ((region (md-toc--region)))
    (delete-region (car region) (cdr region))))

;;;###autoload
(defun md-toc-update ()
  "Update markdown TOC if exists."
  (interactive nil md-ts-mode)
  (when-let* ((region (md-toc--region))
              ((not (md-toc--cache-latest-p))))
    (save-excursion
      (md-toc-delete)
      (goto-char (car region))
      (md-toc-insert))))

;;;###autoload
(defun md-toc-follow-link-at-point ()
  "On a TOC link, navigate to the related markdown header."
  (interactive nil md-ts-mode)
  (when-let* ((region (md-toc--region))
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
(defun md-toc-auto-update ()
  "Auto update markdown TOC before saved."
  (add-hook 'before-save-hook #'md-toc-update t))

(provide 'md-toc)
;;; md-toc.el ends here
