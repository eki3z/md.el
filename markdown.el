;;; markdown.el --- Major mode for editing MARKDOWN files using tree-sitter -*- lexical-binding: t; -*-

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

;; SEE https://www.markdownguide.org/cheat-sheet/
;; TODO impleting highlight, heading ID,  DEfiniton list, emoji, subscript, superscript

;; BUG escape delimiter failed in code inline

;; Features
;;
;; * Indent
;; * IMenu
;; * Navigation
;; * Which-function
;; * Flymake
;; * Tree-sitter parser installation helper
;; * PHP built-in server support
;; * Shell interaction: execute PHP code in a inferior PHP process

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

(defface markdown-ordered-list
  '((t (:inherit font-lock-string-face)))
  "Face for order list item markers."
  :group 'markdown-ts-faces)

(defface markdown-unordered-list
  '((t (:inherit font-lock-number-face)))
  "Face for unorder list item markers."
  :group 'markdown-ts-faces)

(defface markdown-task-list
  '((t (:inherit markdown-unordered-list)))
  "Face for unorder list item markers."
  :group 'markdown-ts-faces)

(defface markdown-blockquote
  '((t (:inherit (italic bold))))
  "Face for blockquote sections."
  :group 'markdown-ts-faces)

(defface markdown-code-inline
  '((t (:inherit (markdown-region font-lock-string-face))))
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
  '((t (:inherit default)))
  "Face for tables."
  :group 'markdown-ts-faces)

(defface markdown-table-delimiter
  '((t (:inherit font-lock-doc-face)))
  "Face for tables."
  :group 'markdown-ts-faces)

(defface markdown-language-info
  '((t (:inherit font-lock-keyword-face)))
  "Face for programming language info strings."
  :group 'markdown-ts-faces)

(defface markdown-link-text
  '((t (:inherit font-lock-number-face)))
  "Face for links."
  :group 'markdown-ts-faces)

(defface markdown-link-url
  '((t (:inherit link)))
  "Face for links."
  :group 'markdown-ts-faces)

(defface markdown-link-title
  '((t (:inherit font-lock-doc-face)))
  "Face for links."
  :group 'markdown-ts-faces)

(defface markdown-link-bracket
  '((t (:inherit markdown-link-title)))
  "Face for HTML comments."
  :group 'markdown-ts-faces)

(defface markdown-reference
  '((t (:inherit font-lock-keyword-face)))
  "Face for link references."
  :group 'markdown-ts-faces)

(defface markdown-footnote
  '((t (:inherit markdown-reference)))
  "Face for link references."
  :group 'markdown-ts-faces)

(defface markdown-comment
  '((t (:inherit (font-lock-comment-face italic))))
  "Face for HTML comments."
  :group 'markdown-ts-faces)

(defface markdown-horizontal-rule
  '((t (:inherit warning)))
  "Face for HTML comments."
  :group 'markdown-ts-faces)

(defface markdown-escape
  '((t (:inherit font-lock-escape-face)))
  "Face for HTML comments."
  :group 'markdown-ts-faces)

;; (defface markdown-line-break
;;   '((t (:inherit font-lock-constant-face :underline t)))
;;   "Face for hard line breaks."
;;   :group 'markdown-ts-faces)

;; (defface markdown-math
;;   '((t (:inherit font-lock-string-face)))
;;   "Face for LaTeX expressions."
;;   :group 'markdown-ts-faces)
;;
(defface markdown-metadata-key
  '((t (:inherit font-lock-property-use-face)))
  "Face for metadata keys."
  :group 'markdown-ts-faces)
;;
(defface markdown-metadata-value
  '((t (:inherit font-lock-string-face)))
  "Face for metadata values."
  :group 'markdown-ts-faces)
;;
;; (defface markdown-gfm-checkbox
;;   '((t (:inherit font-lock-builtin-face)))
;;   "Face for GFM checkboxes."
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
(defface markdown-html-entity
  '((t (:inherit font-lock-variable-name-face)))
  "Face for HTML entities."
  :group 'markdown-ts-faces);;



;;; Syntax table

(defvar markdown--treesit-syntax-table
  (let ((table (make-syntax-table)))
    table)
  "Syntax table for markdown files.")



;;; Indent

(defvar markdown--treesit-indent-rules
  nil)


;;; Font-lock

(defvar markdown--treesit-font-lock-settings
  (treesit-font-lock-rules

   :language 'markdown
   :feature 'horizontal_rule
   '((thematic_break) @markdown-horizontal-rule)

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
   :feature 'blockquote
   '(
     (block_quote)  @markdown-blockquote
     ;; TODO more precise font lock
     ;; (block_quote (block_quote_marker) @markdown-blockquote)
     ;; (block_quote (paragraph (inline (block_continuation) @markdown-blockquote)))
     )

   :language 'markdown
   :feature 'table
   '((pipe_table_header (pipe_table_cell) @markdown-table-header)
     (pipe_table_row (pipe_table_cell) @markdown-table-content)
     (pipe_table (_ "|" @markdown-table-delimiter))
     (pipe_table_delimiter_cell "-" @markdown-table-delimiter))

   :language 'markdown
   :feature 'ordered_list
   '([(list_marker_dot)
      (list_marker_parenthesis)]
     @markdown-ordered-list)

   :language 'markdown
   :feature 'unordered_list
   '([(list_marker_plus)
      (list_marker_minus)
      (list_marker_star)]
     @markdown-unordered-list)

   :language 'markdown
   :feature 'task_list
   '([(task_list_marker_checked)
      (task_list_marker_unchecked)]
     @markdown-task-list)

   :language 'markdown
   :feature 'reference
   '((link_reference_definition
      (link_label) @markdown-reference
      (link_destination) @markdown-link-url)
     (link_reference_definition
      (link_title) @markdown-link-title))

   :language 'markdown
   :feature 'code_block
   :override 'append
   '(
     ;; (fenced_code_block) @markdown-region
     (fenced_code_block_delimiter) @markdown-delimiter
     (info_string) @markdown-language-info
     ;; TODO use injection
     (code_fence_content) @font-lock-string-face
     ;; TODO indented code block
     )

   :language 'markdown-inline
   :feature 'escape
   '((backslash_escape) @markdown-escape)

   :language 'markdown-inline
   :feature 'footnote
   '((shortcut_link
      (link_text) @text
      (:match "^\\^" @text))
     @markdown-footnote)

   :language 'markdown-inline
   :feature 'bold
   :override 'append
   '((strong_emphasis) @markdown-bold)

   :language 'markdown-inline
   :feature 'italic
   :override 'append
   '((emphasis) @markdown-italic)

   :language 'markdown-inline
   :feature 'strikethrough
   :override 'append
   '((strikethrough) @markdown-strikethrough)

   :language 'markdown-inline
   :feature 'code_inline
   :override t
   '((code_span
      (_) @delimiter
      (:equal "`" @delimiter))
     @markdown-code-inline)

   :language 'markdown-inline
   :feature 'delimiter
   :override t
   '((emphasis_delimiter) @markdown-delimiter
     (code_span_delimiter) @markdown-delimiter)

   :language 'markdown-inline
   :feature 'link
   '((inline_link
      (link_text) @markdown-link-text
      (link_destination) @markdown-link-url)
     (inline_link (link_title) @markdown-link-title)
     (inline_link ["[" "]" "(" ")"] @markdown-link-bracket)

     (full_reference_link
      (link_text) @markdown-link-text
      (link_label) @markdown-reference)
     (full_reference_link ["[" "]"] @markdown-link-bracket)

     (uri_autolink) @markdown-link-url)

   :language 'markdown-inline
   :feature 'image
   '((image (image_description) @markdown-link-text)
     (image (link_destination) @markdown-link-url)
     (image (link_title) @markdown-link-title)
     (image (link_label) @markdown-reference)
     (image ["[" "]" "(" ")" "!"] @markdown-link-bracket))

   :language 'html
   :feature 'comment
   '(
     ;; ((html_tag) @text (:match "^<!--.*-->$" @text))
     (comment)
     @markdown-comment)


   :language 'html
   :feature 'html_tag
   '((element) @markdown-html-entity)

   :language 'yaml
   :feature 'metadata
   '([(boolean_scalar) (null_scalar)] @font-lock-constant-face
     (block_mapping_pair
      key: (_) @markdown-metadata-key
      value: (_) @markdown-metadata-value)
     (flow_mapping
      (_ key: (_) @markdown-metadata-key
         value: (_) @markdown-metadata-value))
     (flow_sequence
      (_ key: (_) @markdown-metadata-key
         value: (_) @markdown-metadata-value)
      ["[" "]"] @font-lock-type-face)
     )

   )
  "")

;; (defun markdown-ts-imenu-node-p (node)
;;   "Check if NODE is a valid entry to imenu."
;;   (equal (treesit-node-type (treesit-node-parent node))
;;          "atx_heading"))
;;
;; (defun markdown-ts-imenu-name-function (node)
;;   "Return an imenu entry if NODE is a valid header."
;;   (let ((name (treesit-node-text node)))
;;     (if (markdown-ts-imenu-node-p node)
;;         (thread-first (treesit-node-parent node)(treesit-node-text))
;;       name)))

;;;###autoload
(define-derived-mode markdown-ts-mode fundamental-mode "markdown-ts"
  "Major mode for editing markdown, powered by tree-sitter."
  :syntax-table markdown--treesit-syntax-table

  (unless (and (treesit-ready-p 'markdown)
               (treesit-ready-p 'markdown-inline))
    (error "Tree-sitter for markdown isn't available.
You can install the parsers with M-x
`markdown-ts-mode-install-parsers'"))

  (setq-local treesit-primary-parser (treesit-parser-create 'markdown))
  ;; (treesit-parser-create 'markdown-inline)

  ;; define the injected parser ranges
  (setq-local treesit-range-settings
              (treesit-range-rules
               :embed 'markdown-inline
               :host 'markdown
               '((inline) @cap)

               :embed 'html
               :host 'markdown
               '((html_block) @cap)

               :embed 'yaml
               :host 'markdown
               '((minus_metadata) @cap)

               :embed 'toml
               :host 'markdown
               '((plus_metadata) @cap)))

  ;; (setq-local treesit-language-at-point-function
  ;;             #'markdown-ts-mode--language-at-point)

  ;; Comments
  (setq-local comment-start "<!-- ")
  (setq-local comment-end " -->")

  ;; TODO
  ;; (setq-local comment-start-skip (rx (seq (syntax comment-start)
  ;;                                        (* (syntax whitespace)))))
  ;;(setq-local comment-end-skip (rx (* (syntax whitespace))
  ;;                                 (group (syntax comment-end))))

  ;; TODO imenu
  ;; (setq-local treesit-simple-imenu-settings
  ;;             '(("Headings"
  ;;                "\\`atx_heading\\'"
  ;;                markdown-ts-imenu-node-p
  ;;                markdown-ts-imenu-name-function)))
  ;; Indent
  (setq-local treesit-simple-indent-rules markdown--treesit-indent-rules)

  ;; Font-lock.
  (setq-local treesit-font-lock-settings markdown--treesit-font-lock-settings)
  (setq-local treesit-font-lock-feature-list
              '((comment delimiter escape html_tag metadata)
                ;; basic syntax
                (heading ordered_list unordered_list bold italic blockquote
                         code_inline horizontal_rule link image reference)
                ;; extended syntax
                (table strikethrough footnote code_block task_list)
                ;; (wiki gfm)
                ))

  (treesit-major-mode-setup))

(if (and (treesit-ready-p 'markdown)
         (treesit-ready-p 'markdown-inline))
    (add-to-list 'auto-mode-alist
                 '("\\.md\\'" . markdown-ts-mode)))

(provide 'markdown)
;;; markdown.el ends here
