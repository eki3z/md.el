;;; md.el --- Major mode for editing MARKDOWN files using tree-sitter -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/eki3z/md.el

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
;; * Navigation
;; * Which-function
;; * Flymake
;; * Tree-sitter parser installation helper

;;; Code:

(require 'treesit)

(declare-function xwidget-webkit-browse-url "xwidget")
(declare-function treesit-parser-create "treesit.c")

;;; Install treesitter language parsers
(defvar md-ts-mode--language-source-alist
  '((markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
    (markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src"))
    (html . ("https://github.com/tree-sitter/tree-sitter-html" "master")))
  "Treesitter language parsers required by `md-ts-mode'.
You can customize this variable if you want to stick to a specific
commit and/or use different parsers.")

(defun md-ts-mode-install-parsers ()
  "Install all the required treesitter parsers.
`md-ts-mode--language-source-alist' defines which parsers to install."
  (interactive)
  (let ((treesit-language-source-alist md-ts-mode--language-source-alist))
    (dolist (item md-ts-mode--language-source-alist)
      (treesit-install-language-grammar (car item)))))

;;; Custom variables

(defgroup md nil
  "Toolchain for markdown related markup language."
  :group 'languages)

(defcustom md-enable-extensions nil
  ""
  :type 'boolean
  :group 'md)


(defcustom md-ts-indent-offset 4
  "Number of spaces for each indentation step in `md-ts-mode'."
  :type 'integer
  :safe 'integerp
  :group 'md)


;;; Syntax table

(defvar md--treesit-syntax-table
  (let ((table (make-syntax-table)))
    ;; Inherit from text-mode syntax table as a base
    (modify-syntax-entry ?\" "." table)  ; Treat " as punctuation (not string delimiter)
    (modify-syntax-entry ?\' "." table)  ; Treat ' as punctuation

    ;; Word constituents (letters, numbers, etc. are inherited)
    (modify-syntax-entry ?- "w" table)   ; Hyphens are part of words (e.g., "well-known")
    (modify-syntax-entry ?_ "w" table)   ; Underscores are part of words (e.g., "_emphasis_")

    ;; Punctuation
    (modify-syntax-entry ?. "." table)   ; Period is punctuation
    (modify-syntax-entry ?, "." table)   ; Comma is punctuation
    (modify-syntax-entry ?! "." table)   ; Exclamation is punctuation
    (modify-syntax-entry ?? "." table)   ; Question mark is punctuation
    (modify-syntax-entry ?: "." table)   ; Colon is punctuation
    (modify-syntax-entry ?\; "." table)  ; Semicolon is punctuation

    ;; Markdown-specific markers
    (modify-syntax-entry ?* "." table)   ; Emphasis (*bold*, *italic*) as punctuation
    (modify-syntax-entry ?# "." table)   ; Heading marker (#) as punctuation
    (modify-syntax-entry ?` "\"" table)  ; Backtick (`) as string delimiter for inline code
    (modify-syntax-entry ?< "." table)   ; < for HTML tags or links as punctuation
    (modify-syntax-entry ?> "." table)   ; > for blockquotes or HTML tags as punctuation
    (modify-syntax-entry ?\[ "(" table)   ; [ for links as open parenthesis
    (modify-syntax-entry ?\] ")" table)   ; ] for links as close parenthesis
    (modify-syntax-entry ?\( "(" table)   ; ( for link URLs as open parenthesis
    (modify-syntax-entry ?\) ")" table)   ; ) for link URLs as close parenthesis
    (modify-syntax-entry ?| "." table)   ; | for tables as punctuation
    (modify-syntax-entry ?+ "." table)   ; + for lists as punctuation
    ;; (modify-syntax-entry ?@ "." table)   ; @ for mentions (if supported) as punctuation

    ;; Whitespace
    (modify-syntax-entry ?\s " " table)  ; Space is whitespace
    (modify-syntax-entry ?\t " " table)  ; Tab is whitespace
    (modify-syntax-entry ?\n ">" table)  ; Newline as comment-end (for paragraph breaks)

    table)
  "Syntax table for `md-ts-mode`.")


;;; Indent

(defvar md--treesit-indent-rules
  `((markdown
     ((node-is "atx_heading") column-0 0)
     ;; List items (e.g., `- item`) indent 2 spaces from parent’s beginning of line
     ((node-is "list_item") parent-bol 0)
     ((parent-is "list_item") parent-bol md-ts-indent-offset)

     ;; Fenced code blocks align with their parent (no extra indent)
     ((node-is "fenced_code_block") parent-bol 0)
     ((node-is "indented_code_block") parent-bol md-ts-indent-offset)

     ;; Paragraphs align with their parent (no indent unless in a list/blockquote)
     ((node-is "paragraph") parent-bol 0)))
  "Tree-sitter indent rules for `md-ts-mode'.")


;;; Font-lock

(defgroup md-ts-faces nil
  "Faces used in Markdown Mode."
  :group 'md
  :group 'faces)

(defface md-header
  '((t (:weight extra-bold)))
  "Face for base header."
  :group 'md-ts-faces)

(defface md-header-1
  '((t (:inherit md-header
        :foreground "#9cdbfb")))
  "Face for header 1st level."
  :group 'md-ts-faces)

(defface md-header-2
  '((t (:inherit md-header
        :foreground "#78bbed")))
  "Face for header 2nd level."
  :group 'md-ts-faces)

(defface md-header-3
  '((t (:inherit md-header
        :foreground "#82a1f1")))
  "Face for header 3rd level."
  :group 'md-ts-faces)

(defface md-header-4
  '((t (:inherit md-header
        :foreground "#6881C2")))
  "Face for header 4th level."
  :group 'md-ts-faces)

(defface md-header-5
  '((t (:inherit md-header
        :foreground "#9ca5cb")))
  "Face for header 5th level."
  :group 'md-ts-faces)

(defface md-header-6
  '((t (:inherit md-header
        :foreground "#757c9e")))
  "Face for header 6th level."
  :group 'md-ts-faces)

(defface md-italic
  '((t (:inherit italic)))
  "Face for italic text."
  :group 'md-ts-faces)

(defface md-bold
  '((t (:inherit bold)))
  "Face for bold text."
  :group 'md-ts-faces)

(defface md-strikethrough
  '((t (:strike-through t)))
  "Face for strikethrough text."
  :group 'md-ts-faces)

(defface md-delimiter
  '((t (:inherit font-lock-delimiter-face)))
  "Face for delimiters."
  :group 'md-ts-faces)

(defface md-region
  '((t (:inherit secondary-selection)))
  "Face for tables."
  :group 'md-ts-faces)

(defface md-ordered-list
  '((t (:inherit font-lock-string-face)))
  "Face for order list item markers."
  :group 'md-ts-faces)

(defface md-unordered-list
  '((t (:inherit font-lock-builtin-face)))
  "Face for unorder list item markers."
  :group 'md-ts-faces)

(defface md-task-list
  '((t (:inherit md-unordered-list)))
  "Face for unorder list item markers."
  :group 'md-ts-faces)

(defface md-blockquote
  '((t (:inherit (italic default))))
  "Face for blockquote sections."
  :group 'md-ts-faces)

(defface md-blockquote-marker
  '((t (:inherit (font-lock-keyword-face italic bold))))
  "Face for blockquote sections."
  :group 'md-ts-faces)

(defface md-code-inline
  '((t (:inherit (md-region font-lock-string-face))))
  "Face for blockquote sections."
  :group 'md-ts-faces)

(defface md-table-header
  '((t (:inherit font-lock-builtin-face)))
  "Face for tables."
  :group 'md-ts-faces)

(defface md-table-content
  '((t (:inherit default)))
  "Face for tables."
  :group 'md-ts-faces)

(defface md-table-delimiter
  '((t (:inherit (md-delimiter bold))))
  "Face for tables."
  :group 'md-ts-faces)

(defface md-language-info
  '((t (:inherit font-lock-constant-face)))
  "Face for programming language info strings."
  :group 'md-ts-faces)

(defface md-codeblock-delimiter
  '((t (:inherit (md-delimiter bold))))
  "Face for programming language info strings."
  :group 'md-ts-faces)

(defface md-link-text
  '((t (:inherit font-lock-function-name-face)))
  "Face for links."
  :group 'md-ts-faces)

(defface md-link-url
  '((t (:inherit link)))
  "Face for links."
  :group 'md-ts-faces)

(defface md-link-title
  '((t (:inherit font-lock-doc-face)))
  "Face for links."
  :group 'md-ts-faces)

(defface md-link-label
  '((t (:inherit font-lock-constant-face)))
  "Face for link references."
  :group 'md-ts-faces)

(defface md-wiki-link
  '((t (:inherit (md-link-text bold))))
  "Face for link references."
  :group 'md-ts-faces)

(defface md-tag
  '((t (:inherit font-lock-type-face)))
  "Face for link references."
  :group 'md-ts-faces)

(defface md-image
  '((t (:inherit (font-lock-string-face bold))))
  "Face for link references."
  :group 'md-ts-faces)

(defface md-footnote
  '((t (:inherit md-link-label)))
  "Face for link references."
  :group 'md-ts-faces)

(defface md-footnote-label
  '((t (:inherit md-link-label)))
  "Face for link references."
  :group 'md-ts-faces)

(defface md-footnote-text
  '((t (:inherit md-link-text)))
  "Face for link references."
  :group 'md-ts-faces)

(defface md-comment
  '((t (:inherit (font-lock-comment-face italic))))
  "Face for HTML comments."
  :group 'md-ts-faces)

(defface md-horizontal-rule
  '((t (:inherit md-delimiter bold)))
  "Face for HTML comments."
  :group 'md-ts-faces)

(defface md-escape
  '((t (:inherit font-lock-escape-face)))
  "Face for HTML comments."
  :group 'md-ts-faces)

(defface md-reference
  '((t (:inherit font-lock-operator-face)))
  "Face for HTML comments."
  :group 'md-ts-faces)

(defface md-html-tag-name
  '((t (:inherit font-lock-function-name-face)))
  "Face for HTML tag names."
  :group 'md-ts-faces)

(defface md-html-attr-name
  '((t (:inherit font-lock-variable-name-face)))
  "Face for HTML attribute names."
  :group 'md-ts-faces)
;;
(defface md-html-attr-value
  '((t (:inherit font-lock-string-face)))
  "Face for HTML attribute values."
  :group 'md-ts-faces)

(defface md-line-break
  '((t (:inherit default)))
  "Face for hard line breaks."
  :group 'md-ts-faces)

;; (defface md-math
;;   '((t (:inherit font-lock-string-face)))
;;   "Face for LaTeX expressions."
;;   :group 'md-ts-faces)

;; (defun md--fontify-metadata (node override start end &rest _)
;;   "Fontify the metadata nodes.
;; For NODE, OVERRIDE, START, and END see `treesit-font-lock-rules'."
;;
;;   )

;; (defun md--fontify-blockquote (node override start end &rest _)
;;   ""
;;
;;   )

(defvar md-ts-default-font-lock-settings
  (treesit-font-lock-rules
   :language 'markdown
   :feature 'horizontal_rule
   '((thematic_break) @md-horizontal-rule)

   :language 'markdown
   :feature 'heading
   '((atx_heading (atx_h1_marker)) @md-header-1
     (atx_heading (atx_h2_marker)) @md-header-2
     (atx_heading (atx_h3_marker)) @md-header-3
     (atx_heading (atx_h4_marker)) @md-header-4
     (atx_heading (atx_h5_marker)) @md-header-5
     (atx_heading (atx_h6_marker)) @md-header-6
     (setext_h1_underline) @md-header-1
     (setext_h2_underline) @md-header-2
     (setext_heading (paragraph) @md-header-1))

   :language 'markdown
   :feature 'blockquote
   :override t
   '((block_quote) @md-blockquote
     (block_quote_marker) @md-blockquote-marker
     ((block_continuation) @md-blockquote-marker
      (:match "^>[> ]*$" @md-blockquote-marker)))

   :language 'markdown
   :feature 'table
   '((pipe_table_header (pipe_table_cell) @md-table-header)
     (pipe_table_row (pipe_table_cell) @md-table-content)
     (pipe_table (_ "|" @md-table-delimiter))
     (pipe_table_delimiter_cell "-" @md-table-delimiter))

   :language 'markdown
   :feature 'list
   '([(list_marker_dot)
      (list_marker_parenthesis)]
     @md-ordered-list
     [(list_marker_plus)
      (list_marker_minus)
      (list_marker_star)]
     @md-unordered-list
     [(task_list_marker_checked)
      (task_list_marker_unchecked)]
     @md-task-list)

   :language 'markdown
   :feature 'link_reference
   '((link_reference_definition
      (link_label) @md-link-label
      (link_destination) @md-link-url
      (link_title) :? @md-link-title))

   :language 'markdown
   :feature 'code_block
   :override 'append
   '((fenced_code_block) @md-region
     (fenced_code_block_delimiter) @md-codeblock-delimiter
     (info_string) @md-language-info
     (code_fence_content) @font-lock-string-face
     (indented_code_block) @md-blockquote
     ;; TODO
     ;; (code_fence_content) @md-fontify-codeblock
     )

   ;; :language 'markdown
   ;; :feature 'metadata
   ;; '([(minus_metadata) (plus_metadata)] @md--fontify-metadata)

   :language 'markdown-inline
   :feature 'escape
   '((backslash_escape) @md-escape)

   :language 'markdown-inline
   :feature 'footnote
   '((shortcut_link
      (link_text) @text
      (:match "^\\^" @text))
     @md-footnote)

   :language 'markdown-inline
   :feature 'emphasis
   :override 'append
   '((strong_emphasis) @md-bold
     (emphasis) @md-italic
     (strikethrough) @md-strikethrough)

   :language 'markdown-inline
   :feature 'code_inline
   :override t
   '(
     ;; TODO rewrite inline
     ;; (code_span
     ;;  :anchor (code_span_delimiter) @md-delimiter
     ;;  ;; _ @md-code-inline
     ;;  (code_span_delimiter) @md-delimiter :anchor)
     (code_span) @md-code-inline
     (code_span_delimiter) @md-delimiter)

   :language 'markdown-inline
   :feature 'delimiter
   :override t
   '((emphasis_delimiter) @md-delimiter
     (hard_line_break) @md-line-break)

   :language 'markdown-inline
   :feature 'reference
   '([(entity_reference)
      (numeric_character_reference)]
     @md-reference)

   :language 'markdown-inline
   :feature 'link
   '((inline_link
      (link_text) @md-link-text
      (link_destination) :? @md-link-url
      (link_title) :? @md-link-title)
     (inline_link ["[" "]" "(" ")"] @md-delimiter)

     (full_reference_link
      (link_text) @md-link-text
      (link_label) @md-link-label)
     (full_reference_link ["[" "]"] @md-delimiter)

     (collapsed_reference_link
      (link_text) @md-link-text)
     (collapsed_reference_link ["[" "]"] @md-delimiter)

     [(uri_autolink) (email_autolink)] @md-link-url)

   :language 'markdown-inline
   :feature 'image
   '((image
      "!" @md-image
      (image_description) @md-link-text
      (link_destination) :? @md-link-url
      (link_title) :? @md-link-title
      (link_label) :? @md-link-label)
     (image ["[" "]" "(" ")"] @md-delimiter)))
  "Tree-sitter Font-lock settings for markdown and inline part.")

(defconst md-ts--extension-font-lock-settings
  (treesit-font-lock-rules
   :language 'markdown-inline
   :feature 'wiki_link
   '((wiki_link (link_destination) @md-link-url
                (link_text) :? @md-wiki-link)
     (wiki_link ["[" "|" "]"] @md-delimiter))

   :language 'markdown-inline
   :feature 'tag
   '((tag) @md-tag))
  "Tree-sitter Font-lock settings for extensions of wiki_link and tags.")

(defconst md-ts--html-font-lock-settings
  (treesit-font-lock-rules
   :language 'html
   :feature 'comment
   '((comment) @md-comment)

   :language 'html
   :feature 'html_tag
   '((tag_name) @md-html-tag-name
     (attribute_name) @md-html-attr-name
     (attribute_value) @md-html-attr-value
     (start_tag ["<" ">"] @md-delimiter)
     (end_tag ["</" ">"] @md-delimiter)))
  "Tree-sitter Font-lock settings for html parser.")

(defconst md-ts--yaml-font-lock-settings
  (treesit-font-lock-rules
   :language 'yaml
   :feature 'metadata_yaml
   '()

   )
  "Tree-sitter Font-lock settings for yaml metadata.")

(defconst md-ts--toml-font-lock-settings
  (treesit-font-lock-rules
   :language 'toml
   :feature 'metadata_toml
   '()

   )
  "Tree-sitter Font-lock settings for toml metadata.")


(defun md-ts-mode--language-at-point (point)
  "Return the language at POINT for `md-ts-mode'."
  (let ((node (treesit-node-at point 'markdown)))
    (if node
        (pcase (treesit-node-type node)
          ("inline" 'markdown-inline)
          ("html_block" 'html)
          (_ 'markdown))
      'markdown)))

(defun md-ts-mode-fontify-footnote ()
  )


;; Imenu

(defun md-ts-heading-name (node)
  "Return the text content of the heading NODE's inline content."
  (when-let* ((parent (treesit-node-parent node))
              (inline (treesit-node-child-by-field-name parent "heading_content"))
              ((string= (treesit-node-type inline) "inline")))
    (treesit-node-text inline)))

(defun md-ts-imenu-headings ()
  "Return a list of Markdown heading rules for imenu integration.
Each rule is a list (NAME PATTERN nil EXTRACTOR) for ATX headings (H1-H6).
NAME is the heading level (e.g., \"H1\"), PATTERN matches Tree-sitter
node types like \"atx_h1_marker\", and EXTRACTOR is `md-ts-heading-name'."
  (let ((heading-levels (number-sequence 1 6))
        rules)
    (dolist (level heading-levels rules)
      (push (list (format "H%d" level)
                  (format "\\`atx_h%d_marker\\'" level)
                  nil
                  #'md-ts-heading-name)
            rules))
    (nreverse rules)))


;; Major mode

;;;###autoload
(define-derived-mode md-ts-mode fundamental-mode "md[ts]"
  "Major mode for editing markdown, powered by tree-sitter."
  :syntax-table md--treesit-syntax-table

  (unless (and (treesit-ready-p 'markdown)
               (treesit-ready-p 'markdown-inline)
               (treesit-ready-p 'html))
    (error "Tree-sitter for markdown isn't available.
You can install the parsers with M-x
`md-ts-mode-install-parsers'"))

  (setq-local treesit-primary-parser (treesit-parser-create 'markdown))

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
               '((plus_metadata) @cap)

               ;; ;; TODO setup latex
               ;; :embed 'latex
               ;; :host 'markdown
               ;; '((latex_block) @cap)

               ;;
               ;; ;; TODO setup mdx
               ))

  (setq-local treesit-language-at-point-function #'md-ts-mode--language-at-point)

  ;; Comments
  (setq-local comment-start "<!--")
  (setq-local comment-end "-->")
  (setq-local comment-start-skip (rx (seq (syntax comment-start)
                                          (* (syntax whitespace)))))
  (setq-local comment-end-skip (rx (seq (* (syntax whitespace))
                                        (syntax comment-end))))

  ;; Imenu
  (setq-local treesit-simple-imenu-settings `(,@(md-ts-imenu-headings)))

  ;; Indent
  (setq-local treesit-simple-indent-rules md--treesit-indent-rules)

  ;; Font-lock.
  (setq-local treesit-font-lock-settings
              (append md-ts--default-font-lock-settings
                      (when md-enable-extensions
                        md-ts--extension-font-lock-settings)
                      (when (treesit-ready-p 'html)
                        md-ts--html-font-lock-settings)
                      (when (treesit-ready-p 'yaml)
                        md-ts--yaml-font-lock-settings)
                      (when (treesit-ready-p 'toml)
                        md-ts--toml-font-lock-settings)))

  (setq-local treesit-font-lock-feature-list
              '((comment delimiter)
                (heading emphasis blockquote list task_list table
                         link link_reference image code_inline code_block
                         horizontal_rule html_tag)
                (footnote metadata escape reference)
                (wiki_link tag)))

  ;; NOTE match footnote features which is missing in treesitter


  (treesit-major-mode-setup))

(if (and (treesit-ready-p 'markdown)
         (treesit-ready-p 'markdown-inline)
         (treesit-ready-p 'html))
    (add-to-list 'auto-mode-alist
                 '("\\.md\\'" . md-ts-mode)))

(provide 'md)
;;; md.el ends here
