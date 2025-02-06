;;; markdown.el --- Summary -*- lexical-binding: t -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/eki3z/markdown.el

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

;; More info here

;;; Code:

(require 'treesit)

(declare-function xwidget-webkit-browse-url "xwidget")
(declare-function treesit-parser-create "treesit.c")

(defgroup markdown nil
  "Toolchain for markdown related markup language."
  :group 'languages)

(defgroup markdown-ts-faces nil
  "Faces used in Markdown Mode."
  :group 'markdown
  :group 'faces)

(defface markdown-header
  '((t (:weight extra-bold)))
  "Face for base header."
  :group 'markdown-ts-faces)

(defface markdown-header-1
  '((t (:inherit markdown-header
        :foreground "#9cdbfb")))
  "Face for header 1st level."
  :group 'markdown-ts-faces)

(defface markdown-header-2
  '((t (:inherit markdown-header
        :foreground "#78bbed")))
  "Face for header 2nd level."
  :group 'markdown-ts-faces)

(defface markdown-header-3
  '((t (:inherit markdown-header
        :foreground "#82a1f1")))
  "Face for header 3rd level."
  :group 'markdown-ts-faces)

(defface markdown-header-4
  '((t (:inherit markdown-header
        :foreground "#6881C2")))
  "Face for header 4th level."
  :group 'markdown-ts-faces)

(defface markdown-header-5
  '((t (:inherit markdown-header
        :foreground "#9ca5cb")))
  "Face for header 5th level."
  :group 'markdown-ts-faces)

(defface markdown-header-6
  '((t (:inherit markdown-header
        :foreground "#757c9e")))
  "Face for header 6th level."
  :group 'markdown-ts-faces)

(defface markdown-delimiter
  '((t (:inherit font-lock-delimiter-face)))
  "Face for delimiters."
  :group 'markdown-ts-faces)

(defface markdown-italic
  '((t (:inherit italic)))
  "Face for italic text."
  :group 'markdown-ts-faces)

(defface markdown-bold
  '((t (:inherit bold)))
  "Face for bold text."
  :group 'markdown-ts-faces)

(defface markdown-strikethrough
  '((t (:strike-through t)))
  "Face for strikethrough text."
  :group 'markdown-ts-faces)

(defface markdown-region
  '((t (:inherit secondary-selection)))
  "Face for tables."
  :group 'markdown-ts-faces)

;; (defface markdown-markup
;;   '((t (:inherit shadow :slant normal :weight normal)))
;;   "Face for markup elements."
;;   :group 'markdown-ts-faces)
;;
;; (defface markdown-header-rule
;;   '((t (:inherit markdown-markup)))
;;   "Base face for headers rules."
;;   :group 'markdown-ts-faces)
;;
;; (defface markdown-header-delimiter
;;   '((t (:inherit markdown-markup)))
;;   "Base face for headers hash delimiter."
;;   :group 'markdown-ts-faces)

(defface markdown-order-list
  '((t (:inherit font-lock-string-face)))
  "Face for order list item markers."
  :group 'markdown-ts-faces)

(defface markdown-unorder-list
  '((t (:inherit font-lock-number-face)))
  "Face for unorder list item markers."
  :group 'markdown-ts-faces)

(defface markdown-task-list
  '((t (:inherit markdown-unorder-list)))
  "Face for unorder list item markers."
  :group 'markdown-ts-faces)

(defface markdown-blockquote
  '((t (:inherit (italic bold))))
  "Face for blockquote sections."
  :group 'markdown-ts-faces)

;; (defface markdown-blockquote-marker
;;   '((t (:inherit (font-lock-builtin-face italic bold))))
;;   "Face for blockquote sections."
;; :group 'markdown-ts-faces)

(defface markdown-table-header
  '((t (:inherit font-lock-keyword-face)))
  "Face for tables."
  :group 'markdown-ts-faces)

(defface markdown-table-content
  '((t (:inherit font-lock-number-face)))
  "Face for tables."
  :group 'markdown-ts-faces)

;; (defface markdown-code
;;   '((t (:inherit secondary-selection)))
;;   "Face for inline code, pre blocks, and fenced code blocks.
;; This may be used, for example, to add a contrasting background to
;; inline code fragments and code blocks."
;;   :group 'markdown-ts-faces)
;;
;; (defface markdown-inline-code
;;   '((t (:inherit (markdown-code-face font-lock-constant-face))))
;;   "Face for inline code."
;;   :group 'markdown-ts-faces)
;;
;; (defface markdown-pre
;;   '((t (:inherit (markdown-code-face font-lock-constant-face))))
;;   "Face for preformatted text."
;;   :group 'markdown-ts-faces)
;;
;; (defface markdown-language-keyword
;;   '((t (:inherit font-lock-type-face)))
;;   "Face for programming language identifiers."
;;   :group 'markdown-ts-faces)
;;
(defface markdown-language-info
  '((t (:inherit font-lock-keyword-face)))
  "Face for programming language info strings."
  :group 'markdown-ts-faces)

(defface markdown-link-title
  '((t (:inherit font-lock-keyword-face)))
  "Face for links."
  :group 'markdown-ts-faces)

(defface markdown-link-url
  '((t (:inherit link)))
  "Face for links."
  :group 'markdown-ts-faces)

(defface markdown-reference
  '((t (:inherit font-lock-number-face)))
  "Face for link references."
  :group 'markdown-ts-faces)

;; (defface markdown-missing-link
;;   '((t (:inherit font-lock-warning-face)))
;;   "Face for missing links."
;;   :group 'markdown-ts-faces)

;; (defface markdown-footnote-marker
;;   '((t (:inherit markdown-markup-face)))
;;   "Face for footnote markers."
;;   :group 'markdown-ts-faces)
;;
;; (defface markdown-footnote-text
;;   '((t (:inherit font-lock-comment-face)))
;;   "Face for footnote text."
;;   :group 'markdown-ts-faces)

;; (defface markdown-url
;;   '((t (:inherit font-lock-string-face)))
;;   "Face for URLs that are part of markup.
;; For example, this applies to URLs in inline links:
;; [link text](http://example.com/)."
;;   :group 'markdown-ts-faces)
;;
;; (defface markdown-plain-url
;;   '((t (:inherit markdown-link-face)))
;;   "Face for URLs that are also links.
;; For example, this applies to plain angle bracket URLs:
;; <http://example.com/>."
;;   :group 'markdown-ts-faces)
;;
;; (defface markdown-link-title
;;   '((t (:inherit font-lock-comment-face)))
;;   "Face for reference link titles."
;;   :group 'markdown-ts-faces)
;;
;; (defface markdown-line-break
;;   '((t (:inherit font-lock-constant-face :underline t)))
;;   "Face for hard line breaks."
;;   :group 'markdown-ts-faces)

(defface markdown-comment
  '((t (:inherit font-lock-comment-face)))
  "Face for HTML comments."
  :group 'markdown-ts-faces)

;; (defface markdown-math
;;   '((t (:inherit font-lock-string-face)))
;;   "Face for LaTeX expressions."
;;   :group 'markdown-ts-faces)
;;
;; (defface markdown-metadata-key
;;   '((t (:inherit font-lock-variable-name-face)))
;;   "Face for metadata keys."
;;   :group 'markdown-ts-faces)
;;
;; (defface markdown-metadata-value
;;   '((t (:inherit font-lock-string-face)))
;;   "Face for metadata values."
;;   :group 'markdown-ts-faces)
;;
;; (defface markdown-gfm-checkbox
;;   '((t (:inherit font-lock-builtin-face)))
;;   "Face for GFM checkboxes."
;;   :group 'markdown-ts-faces)
;;
;; (defface markdown-highlight
;;   '((t (:inherit highlight)))
;;   "Face for mouse highlighting."
;;   :group 'markdown-ts-faces)
;;
;; (defface markdown-hr
;;   '((t (:inherit markdown-markup-face)))
;;   "Face for horizontal rules."
;;   :group 'markdown-ts-faces)
;;
;; (defface markdown-html-tag-name
;;   '((t (:inherit font-lock-type-face)))
;;   "Face for HTML tag names."
;;   :group 'markdown-ts-faces)
;;
;; (defface markdown-html-tag-delimiter
;;   '((t (:inherit markdown-markup-face)))
;;   "Face for HTML tag delimiters."
;;   :group 'markdown-ts-faces)
;;
;; (defface markdown-html-attr-name
;;   '((t (:inherit font-lock-variable-name-face)))
;;   "Face for HTML attribute names."
;;   :group 'markdown-ts-faces)
;;
;; (defface markdown-html-attr-value
;;   '((t (:inherit font-lock-string-face)))
;;   "Face for HTML attribute values."
;;   :group 'markdown-ts-faces)
;;
;; (defface markdown-html-entity
;;   '((t (:inherit font-lock-variable-name-face)))
;;   "Face for HTML entities."
;;   :group 'markdown-ts-faces)
;;
;; (defface markdown-highlighting
;;   '((t (:background "yellow" :foreground "black")))
;;   "Face for highlighting."
;;   :group 'markdown-ts-faces)


;;; markdown-ts-mode config

;; (defface markdown-header-1
;;
;;   SPEC
;;   "DOC")


(defvar markdown--treesit-syntax-table
  (let ((table (make-syntax-table)))
    table)
  "Syntax table for markdown files.")

(defvar markdown--treesit-font-lock-settings
  (treesit-font-lock-rules

   :language 'markdown
   :feature 'horizontal_rule
   '((thematic_break) @markdown-comment)

   :language 'markdown
   :feature 'blockquote
   '(
     ;; TODO more precise font lock
     ;; (block_quote_marker) @markdown-blockquote-marker
     ;; (block_quote (paragraph (inline (block_continuation) @markdown-blockquote-marker)))
     (block_quote) @markdown-blockquote)

   :language 'markdown
   :feature 'table
   '((pipe_table
      (pipe_table_header (pipe_table_cell) @markdown-table_header)
      (pipe_table_row (pipe_table_cell) @markdown-table_content)))


   :language 'markdown
   :feature 'heading
   '((atx_heading (atx_h1_marker)) @markdown-header-1
     (atx_heading (atx_h2_marker)) @markdown-header-2
     (atx_heading (atx_h3_marker)) @markdown-header-3
     (atx_heading (atx_h4_marker)) @markdown-header-4
     (atx_heading (atx_h5_marker)) @markdown-header-5
     (atx_heading (atx_h6_marker)) @markdown-header-6
     (setext_h1_underline) @markdown-header-1
     (setext_h2_underline) @markdown-header-2
     (setext_heading (paragraph) @markdown-header-1))

   :language 'markdown
   :feature 'list
   '([(list_marker_plus)
      (list_marker_minus)
      (list_marker_star)]
     @markdown-unorder-list
     [(list_marker_dot)
      (list_marker_parenthesis)]
     @markdown-order-list
     [(task_list_marker_checked)
      (task_list_marker_unchecked)]
     @markdown-task-list)

   :language 'markdown
   :feature 'link_ref
   '((link_reference_definition
      (link_label) @markdown-reference
      (link_destination) @markdown-link-url
      (link_title) @markdown-comment))

   :language 'markdown
   :feature 'code_block
   :override 'append
   '((fenced_code_block) @markdown-region
     (fenced_code_block_delimiter) @markdown-delimiter
     (info_string) @markdown-language-info
     ;; TODO use injection
     (code_fence_content) @font-lock-string-face
     ;; TODO indented code block
     )

   :language 'markdown-inline
   :feature 'emphasis
   :override 'append
   '((emphasis) @markdown-italic
     (strong_emphasis) @markdown-bold
     (strikethrough) @markdown-strikethrough)

   :language 'markdown-inline
   :feature 'code_inline
   :override t
   '((code_span) @markdown-code)

   :language 'markdown-inline
   :feature 'delimiter
   :override 'append
   '((emphasis_delimiter) @markdown-delimiter
     (code_span_delimiter) @markdown-delimiter)

   :language 'markdown-inline
   :feature 'link
   '(

     ;; (link_text) @font-lock-number-face
     ;; inline-link
     (inline_link (link_text) @markdown-link-title)
     (inline_link (link_destination) @markdown-link-url)
     ;; (inline_link ["[" "]" "(" ")"] @markdown-comment)

     (full_reference_link (link_text) @markdown-link-title)
     (full_reference_link (link_label) @markdown-reference)

     ;; image
     (image (image_description) @markdown-link-title)
     (image (link_destination) @markdown-link)
     (image (link_label) @markdown-reference)
     (image ["[" "]" "(" ")" "!"] @markdown-comment))

   ;; :language 'markdown-inline
   ;; :feature 'html_tag
   ;; '((html_tag) @markdown-html-entity)

   )
  "")

(defvar markdown--treesit-indent-rules
  nil)

;;;###autoload
(define-derived-mode markdown-ts-mode fundamental-mode "markdown-ts"
  ""
  :syntax-table markdown--treesit-syntax-table

  (unless (and (treesit-ready-p 'markdown)
               (treesit-ready-p 'markdown-inline))
    (error "Tree-sitter for markdown isn't available"))

  (setq-local treesit-primary-parser (treesit-parser-create 'markdown))
  ;; (treesit-parser-create 'markdown)
  (treesit-parser-create 'markdown-inline)

  ;; Comments

  ;; Indent
  (setq-local treesit-simple-indent-rules markdown--treesit-indent-rules)

  ;; Font-lock.
  (setq-local treesit-font-lock-settings markdown--treesit-font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((horizontal_rule delimiter)
                (heading list link
                         link_ref
                         blockquote emphasis
                         code_inline code_block
                         ;; html_tag
                         table)))

  (treesit-major-mode-setup))

(if (and (treesit-ready-p 'markdown)
         (treesit-ready-p 'markdown-inline))
    (add-to-list 'auto-mode-alist
                 '("\\.md\\'" . markdown-ts-mode)))

(provide 'markdown)
;;; markdown.el ends here
