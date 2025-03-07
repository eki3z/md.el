;;; md-ts-mode.el --- Major mode editing MARKDOWN file using tree-sitter  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Eki Zhang

;; Author: Eki Zhang <liuyinz95@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: language tree-sitter
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

;; SEE https://www.markdownguide.org/cheat-sheet/

;; TODO supports more md features
;; * highlight
;; * heading ID
;; * definition list
;; * emoji
;; * subscript
;; * superscript

;; TODO supports more functions
;; * md-indent-function
;; * navigation, reference,footnote jump
;; * markdown view mode

;; BUG
;; * escape delimiter parsed failed in code inline
;; * html parsed failed sometimes in code inline

;;; Code:

(require 'seq)
(require 'pcase)
(require 'crm)
(require 'treesit)

(declare-function xwidget-webkit-browse-url "xwidget")
(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-parent "treesit.c")
(declare-function treesit-node-child-by-field-name "treesit.c")

;;; Install tree-sitter language parser
(defvar md-ts-mode--language-source-alist
  '((markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
    (markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src"))
    (html . ("https://github.com/tree-sitter/tree-sitter-html" "master"))
    (yaml . ("https://github.com/tree-sitter-grammars/tree-sitter-yaml" "master"))
    (toml . ("https://github.com/tree-sitter-grammars/tree-sitter-toml" "master")))
  "Tree-sitter language parsers required by `md-ts-mode'.
You can customize this variable if you want to stick to a specific
commit and/or use different parsers.")

(defun md-ts-mode-install-parsers ()
  "Install all the required tree-sitter parsers.
`md-ts-mode-language-source-alist' defines which parsers to install."
  (interactive)
  (let ((treesit-language-source-alist md-ts-mode--language-source-alist)
        (parsers (mapcar #'car md-ts-mode--language-source-alist)))
    (when-let* ((to-install (completing-read-multiple
                             "Install parsers: "
                             (mapcar #'symbol-name parsers))))
      (mapcar #'treesit-install-language-grammar
              (mapcar #'intern to-install)))))

;;; Custom variables

(defgroup md-ts nil
  "Package for markdown related markup language."
  :prefix "md-ts-"
  :group 'text
  :link '(url-link "ttps://github.com/eki3z/md"))

(defcustom md-ts-mode-enable-extensions nil
  "If non-nil, enable all features provided by tree-sitter extensions.
Wiki_link and tags are included."
  :type 'boolean
  :safe 'booleanp
  :group 'md-ts)

(defcustom md-ts-mode-indent-offset 3
  "Number of spaces for each indentation step in `md-ts-mode'."
  :type 'natnum
  :safe 'natnump
  :group 'md-ts)


;;; Syntax table

(defvar md-ts-mode--syntax-table
  (let ((st (make-syntax-table text-mode-syntax-table)))
    ;; Inherit from text-mode syntax table as a base
    (modify-syntax-entry ?\" "." st)  ; Treat " as punctuation (not string delimiter)
    (modify-syntax-entry ?\' "." st)  ; Treat ' as punctuation
    ;; Word constituents (letters, numbers, etc. are inherited)
    (modify-syntax-entry ?- "w" st)   ; Hyphens are part of words (e.g., "well-known")
    (modify-syntax-entry ?_ "w" st)   ; Underscores are part of words (e.g., "_emphasis_")
    ;; Markdown-specific markers
    (modify-syntax-entry ?* "." st)   ; Emphasis (*bold*, *italic*) as punctuation
    (modify-syntax-entry ?# "." st)   ; Heading marker (#) as punctuation
    (modify-syntax-entry ?` "\"" st)  ; Backtick (`) as string delimiter for inline code
    (modify-syntax-entry ?< "." st)   ; < for HTML tags or links as punctuation
    (modify-syntax-entry ?> "." st)   ; > for blockquotes or HTML tags as punctuation
    (modify-syntax-entry ?\[ "(" st)   ; [ for links as open parenthesis
    (modify-syntax-entry ?\] ")" st)   ; ] for links as close parenthesis
    (modify-syntax-entry ?\( "(" st)   ; ( for link URLs as open parenthesis
    (modify-syntax-entry ?\) ")" st)   ; ) for link URLs as close parenthesis
    (modify-syntax-entry ?| "." st)   ; | for tables as punctuation
    (modify-syntax-entry ?+ "." st)   ; + for lists as punctuation
    (modify-syntax-entry ?\n ">" st)  ; Newline as comment-end (for paragraph breaks)
    st)
  "Syntax table for `md-ts-mode'.")


;;; Indent

(defvar md-ts-mode--indent-rules
  `((markdown
     ((node-is "minus_metadata") column-0 0)
     ((node-is "plus_metadata") column-0 0)
     ((node-is "atx_heading") column-0 0)

     ;; NOTE usually 0 -3 space is allowed in horizontal rule
     ((node-is "thematic_break") column-0 0)

     ((parent-is "list_item") parent-bol md-ts-mode-indent-offset)
     ((node-is "list_item") parent-bol 0)
     ((node-is "paragraph") parent-bol 0)
     ((node-is "fenced_code_block") parent-bol 0)
     ((node-is "indented_code_block") parent-bol 4)))
  "Tree-sitter indent rules for `md-ts-mode'.")


;;; Font-lock

(defgroup md-ts-faces nil
  "Faces used in `md-ts-mode'."
  :group 'md-ts
  :group 'faces)

(defface md-ts-header-base
  '((t (:weight extra-bold)))
  "Face for header base properties.."
  :group 'md-ts-faces)

(defface md-ts-header-1
  '((t (:inherit (md-ts-header-base outline-1))))
  "Face for header 1st level."
  :group 'md-ts-faces)

(defface md-ts-header-2
  '((t (:inherit (md-ts-header-base outline-2))))
  "Face for header 2nd level."
  :group 'md-ts-faces)

(defface md-ts-header-3
  '((t (:inherit (md-ts-header-base outline-3))))
  "Face for header 3rd level."
  :group 'md-ts-faces)

(defface md-ts-header-4
  '((t (:inherit (md-ts-header-base outline-4))))
  "Face for header 4th level."
  :group 'md-ts-faces)

(defface md-ts-header-5
  '((t (:inherit (md-ts-header-base outline-5))))
  "Face for header 5th level."
  :group 'md-ts-faces)

(defface md-ts-header-6
  '((t (:inherit (md-ts-header-base outline-6))))
  "Face for header 6th level."
  :group 'md-ts-faces)

(defface md-ts-strikethrough
  '((t (:strike-through t)))
  "Face for strikethrough text."
  :group 'md-ts-faces)

(defface md-ts-delimiter
  '((t (:inherit font-lock-delimiter-face)))
  "Face for delimiters."
  :group 'md-ts-faces)

(defface md-ts-ordered-list
  '((t (:inherit font-lock-string-face)))
  "Face for order list item markers."
  :group 'md-ts-faces)

(defface md-ts-unordered-list
  '((t (:inherit font-lock-builtin-face)))
  "Face for unordered list item markers."
  :group 'md-ts-faces)

(defface md-ts-task-list
  '((t (:inherit md-ts-unordered-list)))
  "Face for task list item markers."
  :group 'md-ts-faces)

(defface md-ts-blockquote
  '((t (:inherit (italic default))))
  "Face for blockquote sections."
  :group 'md-ts-faces)

(defface md-ts-blockquote-marker
  '((t (:inherit (font-lock-builtin-face italic bold))))
  "Face for blockquote markers."
  :group 'md-ts-faces)

(defface md-ts-code-block
  '((t (:inherit secondary-selection)))
  "Face for code block section."
  :group 'md-ts-faces)

(defface md-ts-code-inline
  '((t (:inherit (md-ts-code-block font-lock-string-face)
        :extend nil)))
  "Face for code inline section."
  :group 'md-ts-faces)

(defface md-ts-code-delimiter
  '((t (:inherit (md-ts-delimiter bold))))
  "Face for code block delimiters."
  :group 'md-ts-faces)

(defface md-ts-code-language
  '((t (:inherit font-lock-constant-face)))
  "Face for code block language info strings."
  :group 'md-ts-faces)

(defface md-ts-table-header
  '((t (:inherit font-lock-builtin-face)))
  "Face for table headers."
  :group 'md-ts-faces)

(defface md-ts-table-delimiter
  '((t (:inherit (md-ts-delimiter bold))))
  "Face for tables delimiter."
  :group 'md-ts-faces)

(defface md-ts-link-text
  '((t (:inherit font-lock-function-name-face)))
  "Face for link text."
  :group 'md-ts-faces)

(defface md-ts-link-url
  '((t (:inherit link)))
  "Face for link url."
  :group 'md-ts-faces)

(defface md-ts-link-title
  '((t (:inherit font-lock-doc-face)))
  "Face for link title."
  :group 'md-ts-faces)

(defface md-ts-link-label
  '((t (:inherit (underline font-lock-constant-face))))
  "Face for link label."
  :group 'md-ts-faces)

(defface md-ts-wiki-link
  '((t (:inherit (md-link-text bold))))
  "Face for wiki link."
  :group 'md-ts-faces)

(defface md-ts-tag
  '((t (:inherit font-lock-type-face)))
  "Face for tag section."
  :group 'md-ts-faces)

(defface md-ts-image-marker
  '((t (:inherit (font-lock-string-face bold))))
  "Face for image link marker."
  :group 'md-ts-faces)

(defface md-ts-footnote-label
  '((t (:inherit (underline font-lock-string-face))))
  "Face for footnote label."
  :group 'md-ts-faces)

(defface md-ts-footnote-text
  '((t (:inherit md-ts-footnote-label)))
  "Face for footnote text."
  :group 'md-ts-faces)

(defface md-ts-comment
  '((t (:inherit (font-lock-comment-face italic))))
  "Face for HTML comments."
  :group 'md-ts-faces)

(defface md-ts-horizontal-rule
  '((t (:inherit (md-ts-delimiter bold))))
  "Face for horizontal rules."
  :group 'md-ts-faces)

(defface md-ts-escape
  '((t (:inherit font-lock-escape-face)))
  "Face for escape characters."
  :group 'md-ts-faces)

(defface md-ts-reference
  '((t (:inherit font-lock-number-face)))
  "Face for HTML comments."
  :group 'md-ts-faces)

(defface md-ts-html-tag-name
  '((t (:inherit font-lock-function-name-face)))
  "Face for HTML tag names."
  :group 'md-ts-faces)

(defface md-ts-html-tag-delimiter
  '((t (:inherit font-lock-number-face)))
  "Face for HTML tag delimiters."
  :group 'md-ts-faces)

(defface md-ts-html-attr-name
  '((t (:inherit font-lock-variable-name-face)))
  "Face for HTML attribute names."
  :group 'md-ts-faces)
;;
(defface md-ts-html-attr-value
  '((t (:inherit font-lock-string-face)))
  "Face for HTML attribute values."
  :group 'md-ts-faces)

(defface md-ts-line-break
  '((t (:inherit default)))
  "Face for hard line breaks."
  :group 'md-ts-faces)

(defvar md-ts-mode--markdown-font-lock-settings
  (treesit-font-lock-rules
   :language 'markdown
   :feature 'horizontal_rule
   '((thematic_break) @md-ts-horizontal-rule)

   :language 'markdown
   :feature 'heading
   '((atx_heading (atx_h1_marker)) @md-ts-header-1
     (atx_heading (atx_h2_marker)) @md-ts-header-2
     (atx_heading (atx_h3_marker)) @md-ts-header-3
     (atx_heading (atx_h4_marker)) @md-ts-header-4
     (atx_heading (atx_h5_marker)) @md-ts-header-5
     (atx_heading (atx_h6_marker)) @md-ts-header-6
     (setext_heading (setext_h1_underline)) @md-ts-header-1
     (setext_heading (setext_h2_underline)) @md-ts-header-2)

   :language 'markdown
   :feature 'blockquote
   :override t
   '((block_quote) @md-ts-blockquote
     (block_quote_marker) @md-ts-blockquote-marker
     ((block_continuation) @cap (:match "^>[> ]*$" @cap)) @md-ts-blockquote-marker)

   :language 'markdown
   :feature 'table
   '((pipe_table_header (pipe_table_cell) @md-ts-table-header)
     (pipe_table (_ "|" @md-ts-table-delimiter))
     (pipe_table_delimiter_cell "-" @md-ts-table-delimiter))

   :language 'markdown
   :feature 'list
   '([(list_marker_dot)
      (list_marker_parenthesis)]
     @md-ts-ordered-list
     [(list_marker_plus)
      (list_marker_minus)
      (list_marker_star)]
     @md-ts-unordered-list
     [(task_list_marker_checked)
      (task_list_marker_unchecked)]
     @md-ts-task-list)

   :language 'markdown
   :feature 'link_reference
   '((link_reference_definition
      (link_label) @md-ts-mode--fontify-label
      (link_destination) @md-ts-link-url
      (link_title) :? @md-ts-link-title))

   :language 'markdown
   :feature 'code_block
   :override 'append
   '((indented_code_block) @md-ts-blockquote
     (fenced_code_block) @md-ts-code-block
     (fenced_code_block_delimiter) @md-ts-code-delimiter
     (info_string (language) @md-ts-code-language)
     ;; TODO rewrite codeblock highlight natively like org-mode
     (code_fence_content) @font-lock-string-face))
  "Tree-sitter Font-lock settings for markdown and inline part.")

(defvar md-ts-mode--markdown-inline-font-lock-settings
  (append
   (treesit-font-lock-rules
    :language 'markdown-inline
    :feature 'escape
    '((backslash_escape) @md-ts-escape)

    :language 'markdown-inline
    :feature 'footnote
    '((shortcut_link
       (link_text) @text
       (:match "^\\^" @text))
      @md-ts-mode--fontify-label)

    :language 'markdown-inline
    :feature 'emphasis
    :override 'append
    '((strong_emphasis) @bold
      (emphasis) @italic
      (strikethrough) @md-ts-strikethrough)

    :language 'markdown-inline
    :feature 'code_inline
    :override t
    '((code_span) @md-ts-code-inline
      (code_span_delimiter) @md-ts-delimiter)

    :language 'markdown-inline
    :feature 'delimiter
    :override t
    '((emphasis_delimiter) @md-ts-delimiter
      (hard_line_break) @md-ts-line-break)

    :language 'markdown-inline
    :feature 'reference
    '([(entity_reference)
       (numeric_character_reference)]
      @md-ts-reference)

    :language 'markdown-inline
    :feature 'link
    '((inline_link
       (link_text) @md-ts-link-text
       (link_destination) :? @md-ts-link-url
       (link_title) :? @md-ts-link-title)
      (inline_link ["[" "]" "(" ")"] @md-ts-delimiter)

      (full_reference_link
       (link_text) @md-ts-link-text
       (link_label) @md-ts-mode--fontify-label)
      (full_reference_link ["[" "]"] @md-ts-delimiter)

      (collapsed_reference_link
       (link_text) @md-ts-link-text)
      (collapsed_reference_link ["[" "]"] @md-ts-delimiter)

      [(uri_autolink) (email_autolink)] @md-ts-link-url)

    :language 'markdown-inline
    :feature 'image
    '((image
       "!" @md-ts-image-marker
       (image_description) @md-ts-link-text
       (link_destination) :? @md-ts-link-url
       (link_title) :? @md-ts-link-title
       (link_label) :? @md-ts-mode--fontify-label)
      (image ["[" "]" "(" ")"] @md-ts-delimiter)))

   (when md-ts-mode-enable-extensions
     (treesit-font-lock-rules
      :language 'markdown-inline
      :feature 'wiki_link
      '((wiki_link (link_destination) @md-ts-link-url
                   (link_text) :? @md-ts-wiki-link)
        (wiki_link ["[" "|" "]"] @md-ts-delimiter))

      :language 'markdown-inline
      :feature 'tag
      '((tag) @md-ts-tag))))
  "Tree-sitter Font-lock settings for markdown-inline parser.")

(defvar md-ts-mode--html-font-lock-settings
  (treesit-font-lock-rules
   :language 'html
   :feature 'comment
   '((comment) @md-ts-comment)

   :language 'html
   :feature 'html_tag
   '((tag_name) @md-ts-html-tag-name
     (attribute_name) @md-ts-html-attr-name
     (attribute_value) @md-ts-html-attr-value
     (start_tag ["<" ">"] @md-ts-html-tag-delimiter)
     (end_tag ["</" ">"] @md-ts-html-tag-delimiter)))
  "Tree-sitter Font-lock settings for html parser.")

(defvar md-ts-mode--yaml-font-lock-settings
  (treesit-font-lock-rules
   :language 'yaml
   :feature 'metadata_yaml
   '((["[" "]" "{" "}"]) @font-lock-bracket-face
     (["," ":" "-" ">" "?" "|"]) @font-lock-delimiter-face
     (["---"]) @md-ts-horizontal-rule

     (block_mapping_pair key: (_) @font-lock-property-use-face)
     (flow_mapping (_ key: (_) @font-lock-property-use-face))
     (flow_sequence (_ key: (_) @font-lock-property-use-face))

     [(alias_name) (anchor_name) (tag)] @font-lock-type-face
     [(block_scalar) (double_quote_scalar) (single_quote_scalar) (string_scalar)]
     @font-lock-string-face
     [(boolean_scalar) (null_scalar) (reserved_directive)
      (tag_directive)(yaml_directive)]
     @font-lock-constant-face
     [(float_scalar) (integer_scalar)] @font-lock-number-face
     (escape_sequence) @font-lock-escape-face))
  "Tree-sitter Font-lock settings for yaml metadata.")

(defvar md-ts-mode--toml-font-lock-settings
  (treesit-font-lock-rules
   :language 'toml
   :feature 'metadata_toml
   :override t
   '((boolean) @font-lock-constant-face
     (["="]) @font-lock-delimiter-face
     [(integer) (float) (local_date) (local_date_time)
      (local_time) (offset_date_time)] @font-lock-number-face
     (string) @font-lock-string-face
     (escape_sequence) @font-lock-escape-face
     [(bare_key) (quoted_key)] @font-lock-property-use-face
     (array [ "[" "]"] @font-lock-bracket-face)
     (table ("[" @font-lock-bracket-face
             (_) @font-lock-type-face
             "]" @font-lock-bracket-face))
     (table_array_element ("[[" @font-lock-bracket-face
                           (_) @font-lock-type-face
                           "]]" @font-lock-bracket-face))
     (table (quoted_key) @font-lock-type-face)
     (table (dotted_key (quoted_key)) @font-lock-type-face)
     ((ERROR) @hr (:equal "+++" @hr)) @md-ts-horizontal-rule))
  "Tree-sitter Font-lock settings for toml metadata.")

(defun md-ts-mode--language-at-point (point)
  "Return the language at POINT for `md-ts-mode'."
  (if-let* ((node (treesit-node-at point 'markdown)))
      (pcase (treesit-node-type node)
        ("minus_metadata" 'yaml)
        ("plus_metadata" 'toml)
        ("html_block" 'html)
        ("inline"
         (if-let* ((node-i (treesit-node-at point 'markdown-inline)))
             (pcase (treesit-node-type node-i)
               ("html_tag" 'html)
               ("latex_block" 'latex)
               (_ 'markdown-inline))
           'markdown-inline))
        (_ 'markdown))
    'markdown))

;; TODO add more embedded langs, latex, mdx
(defvar md-ts-mode--embedded-languages '(markdown-inline html yaml toml latex))
(defun md-ts-mode--get-font-lock ()
  "Return available font lock settings for `md-ts-mode'."
  (thread-last
    md-ts-mode--embedded-languages
    (cons 'markdown)
    (seq-filter (lambda (x) (treesit-ready-p x 'quiet)))
    (seq-mapcat (lambda (x)
                  (symbol-value
                   (intern (format "md-ts-mode--%s-font-lock-settings"
                                   (symbol-name x))))))))

(defun md-ts-mode--get-fenced-language (node)
  "Return the language info of fenced code block NODE located."
  (when-let* ((block (treesit-node-parent node))
              (info (car (treesit-filter-child
                          block
                          (lambda (n) (string= (treesit-node-type n) "info_string"))
                          t)))
              (lang (car (treesit-filter-child
                          info
                          (lambda (n) (string= (treesit-node-type n) "language"))
                          t))))
    (treesit-node-text lang 'no-property)))

(defun md-ts-mode--fontify-label (node override start end &rest _)
  "Function to fonfity matched link lable or footnote label.
NODE, OVERRIDE, START, END please refer to `font-lock-keywords'."
  (let* ((n-start (treesit-node-start node))
         (n-end (treesit-node-end node))
         (footnote-p (char-equal ?^ (char-after (1+ n-start))))
         (label-face (if footnote-p 'md-ts-footnote-label 'md-ts-link-label)))
    (mapc (pcase-lambda (`(,ns ,ne ,nf))
            (treesit-fontify-with-override ns ne nf override start end))
          `((,n-start ,(1+ n-start) md-ts-delimiter)
            (,(1+ n-start) ,(1- n-end) ,label-face)
            (,(1- n-end) ,n-end md-ts-delimiter)))))

(defvar md-ts-mode--embedded-range-rules
  '((:embed 'markdown-inline
     :host 'markdown
     '((inline) @cap
       (pipe_table_row (pipe_table_cell) @cap)))

    (:embed 'html
     :host 'markdown
     '((html_block) @cap))

    ;; FIXME failed to capture html in inline sometimes
    (:embed 'html
     :host 'markdown-inline
     '((html_tag) @cap))

    (:embed 'yaml
     :host 'markdown
     '((minus_metadata) @cap))

    (:embed 'toml
     :host 'markdown
     '((plus_metadata) @cap))

    ;; TODO identify language by language info-string
    ;; (:embed #'md-ts-mode--get-fenced-language
    ;;  :host 'markdown
    ;;  '((code_fence_content) @language))
    ))

(defmacro md-ts-mode--get-range-rules ()
  "Return embedded range rules for `md-ts-mode'."
  `(treesit-range-rules
    ,@(thread-last
        md-ts-mode--embedded-range-rules
        (seq-filter
         (lambda (x)
           (and (treesit-ready-p (cadr (plist-get x :embed)) 'quiet)
                (treesit-ready-p (cadr (plist-get x :host)) 'quiet))))
        (apply #'seq-concatenate 'list))))

(defun md-ts-mode--enabled-feature ()
  "Return a list of features which are enabled in `md-ts-mode'."
  (seq-keep (lambda (s)
              (and (treesit-font-lock-setting-enable s)
                   (treesit-font-lock-setting-feature s)))
            treesit-font-lock-settings))

;; (defun md-ts-mode--footnote-content-matcher (limit)
;;   "Find and set match data for footnote content up to LIMIT."
;;   (when-let* ((inline-root (treesit-buffer-root-node 'markdown-inline))
;;               (shortcuts (treesit-filter-child
;;                           inline-root
;;                           (lambda(x)
;;                             (and (string= (treesit-node-type x) "shortcut_link")
;;                                  (string-match-p "^\\^" (treesit-node-text x) 1))))))
;;     (save-excursion
;;       (while (and shortcuts (< (point) limit))
;;         (goto-char (treesit-node-start (pop shortcuts)))
;;         (beginning-of-line)
;;         (when (looking-at "\\s-*\\[^[^]]+\\]:\\s-+\\(.*\\)$")
;;           (let ((start (match-beginning 1))
;;                 (end (match-end 1)))
;;             (when (< end limit)
;;               (set-match-data (list start end)) t)))))))

(defun md-ts-mode--fontify-region-hybridly (start end &optional loudly)
  "Fontify markdown buffer with both tree-sitter and traditonal keywords.
START, END, LOUDLY is same with"
  (treesit-font-lock-fontify-region start end loudly)
  (font-lock-fontify-keywords-region start end loudly))


;; Imenu

(defun md-ts-mode--heading-name (node)
  "Return the text content of the heading NODE's inline content."
  (when-let* ((parent (treesit-node-parent node))
              (inline (treesit-node-child-by-field-name parent "heading_content"))
              ((string= (treesit-node-type inline) "inline")))
    (treesit-node-text inline)))

(defun md-ts-mode--imenu-headings ()
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
                  #'md-ts-mode--heading-name)
            rules))
    (nreverse rules)))


;; Navigation


;; Major mode

;;;###autoload
(define-derived-mode md-ts-mode fundamental-mode "MD"
  "Major mode for editing markdown, powered by tree-sitter."
  :group 'md
  :syntax-table md-ts-mode--syntax-table

  (unless (treesit-ready-p 'markdown 'quiet)
    (error "Tree-sitter for markdown isn't available.
You can install the parser with M-x `md-ts-mode-install-parsers'"))

  ;; Comments
  (setq-local comment-start "<!--")
  (setq-local comment-end "-->")
  (setq-local comment-start-skip (rx (seq (syntax comment-start)
                                          (* (syntax whitespace)))))
  (setq-local comment-end-skip (rx (seq (* (syntax whitespace))
                                        (syntax comment-end))))

  ;; Indent
  (setq-local treesit-simple-indent-rules md-ts-mode--indent-rules)

  ;; Font-lock.
  (setq-local treesit-primary-parser (treesit-parser-create 'markdown))

  ;; FIXME range settings should create parser automatically
  (dolist (lang md-ts-mode--embedded-languages)
    (when (treesit-ready-p lang 'quiet)
      (treesit-parser-create lang)))

  ;; NOTE maybe do not need to select rules, because it doesn't create parser
  ;; automatically
  (setq-local treesit-range-settings (md-ts-mode--get-range-rules))
  (setq-local treesit-font-lock-settings (md-ts-mode--get-font-lock))
  (setq-local font-lock-fontify-region-function #'md-ts-mode--fontify-region-hybridly)

  ;; ;; add more font-lock features which are not supported in md grammar
  ;; (font-lock-add-keywords
  ;;  nil
  ;;  `(;; markdown parser do not have footnote node type, so use font-lock-keywords
  ;;    ;; to add highlight for footnote text
  ;;    ,(when (memq 'footnote (md-ts-mode--enabled-feature))
  ;;       (list #'md-ts-mode--footnote-content-matcher 0 'md-ts-footnote-text t))))

  (setq-local treesit-language-at-point-function #'md-ts-mode--language-at-point)
  (setq-local treesit-font-lock-feature-list
              '((comment delimiter)
                (heading emphasis blockquote list task_list table
                         link link_reference image code_inline code_block
                         horizontal_rule html_tag)
                (escape reference footnote metadata_yaml metadata_toml)
                (wiki_link tag)))

  ;; Imenu
  (setq-local treesit-simple-imenu-settings `(,@(md-ts-mode--imenu-headings)))

  ;; Outline
  (setq-local treesit-outline-predicate "section")

  ;; ;; Navigation
  ;; (setq-local treesit-defun-type-regexp
  ;;             (rx (or "pair" "object")))
  ;; (setq-local treesit-defun-name-function #'md-ts-mode--defun-name)
  ;; (setq-local treesit-thing-settings
  ;;             `((markdown
  ;;                (sentence "pair"))))

  (treesit-major-mode-setup))

(derived-mode-add-parents 'md-ts-mode '(markdown-mode))

(when (treesit-ready-p 'markdown 'quiet)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . md-ts-mode)))

(provide 'md-ts-mode)

;;; md-ts-mode.el ends here
