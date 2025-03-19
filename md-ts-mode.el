;;; md-ts-mode.el --- Major mode editing MARKDOWN file using tree-sitter  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Eki Zhang

;; Author: Eki Zhang <liuyinz95@gmail.com>
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

;; TODO supports more md features
;; * highlight
;; * heading ID
;; * definition list
;; * emoji
;; * subscript
;; * superscript

;; TODO supports more functions
;; * markdown view mode

;;; Code:

(require 'seq)
(require 'pcase)
(require 'crm)
(require 'mailcap)
(require 'dnd)

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

(defcustom md-ts-mode-enable-extensions nil
  "If non-nil, enable all features provided by tree-sitter extensions.
Wiki_link and tags are included."
  :type 'boolean
  :safe 'booleanp
  :group 'md)

(defcustom md-ts-mode-indent-offset 3
  "Number of spaces for each indentation step in `md-ts-mode'."
  :type 'natnum
  :safe 'natnump
  :group 'md)

(defcustom md-ts-mode-enable-setext-heading t
  "If non-nil, enable setext style headings in `md-ts-mode'."
  :type 'boolean
  :safe 'booleanp
  :group 'md)

(defcustom md-ts-mode-fontify-heading-markers-only nil
  "If non-nil, fontify level markers only in headings."
  :type 'boolean
  :safe 'booleanp
  :group 'md)

(defcustom md-ts-mode-fontify-fenced-blocks-natively nil
  "When non-nil, fontify code in code blocks using the native major mode.
This only works for fenced code blocks where the language is
specified where we can automatically determine the appropriate
mode to use.  The language to mode mapping may be customized by
setting the variable `md-ts-mode-language-mappings'."
  :type 'boolean
  :safe 'booleanp
  :group 'md)

(defcustom md-ts-mode-language-mappings
  '((nil text-mode)
    (("asymptote" "asy") asy-mode)
    (("bash" "shell" "sh") bash-ts-mode sh-mode)
    (("c") c-ts-mode c-mode)
    (("cpp" "c++") c++-ts-mode c++-mode)
    (("csharp" "c#") csharp-ts-mode)
    (("css") css-ts-mode css-mode)
    (("d2") d2-ts-mode d2-mode)
    (("dart") dart-ts-mode)
    (("ditaa") artist-mode)
    (("elisp" "emacs-lisp") emacs-lisp-mode)
    (("elixir") elixir-ts-mode)
    (("go") go-ts-mode)
    (("html") mhtml-ts-mode html-ts-mode html-mode)
    (("javascript" "js") js-ts-mode js-mode js2-mode)
    (("java") java-ts-mode)
    (("jq") jq-ts-mode)
    (("json") json-ts-mode js-json-mode)
    (("jsx") js-jsx-mode)
    (("lua") lua-ts-mode lua-mode)
    (("markdown" "md") md-ts-mode markdown-mode)
    (("mermaid") mermaid-ts-mode)
    (("ocaml") tuareg-mode)
    (("php") php-ts-mode php-mode)
    (("python" "py") python-ts-mode python-mode)
    (("ruby") ruby-ts-mode)
    (("rust") rust-ts-mode)
    (("screen") shell-script-mode)
    (("sqlite") sql-mode)
    (("swift") swift-ts-mode)
    (("toml") toml-ts-mode toml-mode)
    (("tsx") tsx-ts-mode)
    (("typescript" "ts") typescript-ts-mode)
    (("typst") typst-ts-mode)
    (("yaml" "yml") yaml-ts-mode yaml-mode)
    (("zig") zig-ts-mode))
  "List of mappings between language aliases and major modes for fenced block.
Each entry is a cons cell of two lists:
- a list of language alias strings, case insensitive.
  e.g. (\\\"py\\\" \\\"python\\\")
- a list of major modes in order of preference.
  e.g. (python-ts-mode python-mode)
When fontifying a code block, the first available mode is used. An entry with
 nil as the alias list serves as a default fallback, applying its modes if no
 other entry matches or if no modes in a matching entry are available"
  :type '(repeat
          (cons
           (choice
            (repeat (string :tag "Language alias"))
            (const :tag "Default (any unmatched language)" nil))
           (repeat (symbol :tag "Major mode"))))
  :group 'md)


;;; Syntax table

(defvar md-ts-mode--syntax-table
  (let ((st (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?\" "." st)  ; Treat " as punctuation (not string delimiter)
    ;; (modify-syntax-entry ?\' "." st)  ; Treat ' as punctuation
    ;; ;; Word constituents (letters, numbers, etc. are inherited)
    ;; (modify-syntax-entry ?- "w" st)   ; Hyphens are part of words (e.g., "well-known")
    ;; (modify-syntax-entry ?_ "w" st)   ; Underscores are part of words (e.g., "_emphasis_")
    ;; ;; Markdown-specific markers
    ;; (modify-syntax-entry ?* "." st)   ; Emphasis (*bold*, *italic*) as punctuation
    ;; (modify-syntax-entry ?# "." st)   ; Heading marker (#) as punctuation
    ;; ;; (modify-syntax-entry ?` "\"" st)  ; Backtick (`) as string delimiter for inline code
    ;; (modify-syntax-entry ?< "." st)   ; < for HTML tags or links as punctuation
    ;; (modify-syntax-entry ?> "." st)   ; > for blockquotes or HTML tags as punctuation
    ;; (modify-syntax-entry ?| "." st)   ; | for tables as punctuation
    ;; (modify-syntax-entry ?+ "." st)   ; + for lists as punctuation
    ;; (modify-syntax-entry ?\n ">" st)  ; Newline as comment-end (for paragraph breaks)
    st)
  "Syntax table for `md-ts-mode'.")


;;; Indent

(defvar md-ts-mode--indent-rules
  `((markdown
     ((node-is "minus_metadata") column-0 0)
     ((node-is "plus_metadata") column-0 0)
     ((node-is "atx_heading") column-0 0)

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
  :group 'md
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

(defface md-ts-delimiter
  '((t (:inherit (font-lock-delimiter-face bold))))
  "Face for delimiters."
  :group 'md-ts-faces)

(defface md-ts-strikethrough
  `((t (:strike-through ,(face-foreground 'font-lock-delimiter-face))))
  "Face for strikethrough text."
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
  '((t (:inherit (italic bold))))
  "Face for blockquote sections."
  :group 'md-ts-faces)

(defface md-ts-blockquote-marker
  '((t (:inherit (md-ts-blockquote font-lock-builtin-face))))
  "Face for blockquote markers."
  :group 'md-ts-faces)

(defface md-ts-code-block
  '((t (:inherit secondary-selection)))
  "Face for code block section."
  :group 'md-ts-faces)

(defface md-ts-indented-block
  '((t (:inherit (md-ts-code-block bold))))
  "Face for indented block section."
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

;; (defface md-ts-line-break
;;   '((t (:inherit default)))
;;   "Face for hard line breaks."
;;   :group 'md-ts-faces)

(defvar md-ts-mode--markdown-font-lock-settings
  (treesit-font-lock-rules
   :language 'markdown
   :feature 'horizontal_rule
   '((thematic_break) @md-ts-horizontal-rule)

   :language 'markdown
   :feature 'heading
   `(,@(if md-ts-mode-fontify-heading-markers-only
           '((atx_heading (atx_h1_marker) @md-ts-header-1)
             (atx_heading (atx_h2_marker) @md-ts-header-2)
             (atx_heading (atx_h3_marker) @md-ts-header-3)
             (atx_heading (atx_h4_marker) @md-ts-header-4)
             (atx_heading (atx_h5_marker) @md-ts-header-5)
             (atx_heading (atx_h6_marker) @md-ts-header-6))
         '((atx_heading (atx_h1_marker)) @md-ts-header-1
           (atx_heading (atx_h2_marker)) @md-ts-header-2
           (atx_heading (atx_h3_marker)) @md-ts-header-3
           (atx_heading (atx_h4_marker)) @md-ts-header-4
           (atx_heading (atx_h5_marker)) @md-ts-header-5
           (atx_heading (atx_h6_marker)) @md-ts-header-6))
     ,@(when md-ts-mode-enable-setext-heading
         (if md-ts-mode-fontify-heading-markers-only
             '((setext_heading (setext_h1_underline) @md-ts-header-1)
               (setext_heading (setext_h2_underline) @md-ts-header-2))
           '((setext_heading (setext_h1_underline)) @md-ts-header-1
             (setext_heading (setext_h2_underline)) @md-ts-header-2))))

   :language 'markdown
   :feature 'blockquote
   :override t
   '((block_quote) @md-ts-blockquote
     (block_quote_marker) @md-ts-blockquote-marker
     ((block_continuation) @cap (:match "^>[> ]*$" @cap)) @md-ts-blockquote-marker)

   :language 'markdown
   :feature 'table
   '((pipe_table_header (pipe_table_cell) @md-ts-table-header)
     (pipe_table (_ ["|"] @md-ts-table-delimiter))
     (pipe_table_delimiter_cell ["-"] @md-ts-table-delimiter))

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
      (link_label) :? @md-ts-mode--fontify-label
      (link_destination) :? @md-ts-link-url
      (link_title) :? @md-ts-link-title))

   :language 'markdown
   :feature 'code_block
   :override 'append
   '((fenced_code_block) @md-ts-code-block
     (fenced_code_block_delimiter) @md-ts-code-delimiter
     (info_string (language) @md-ts-code-language)
     (code_fence_content) @md-ts-mode--fontify-fenced-block
     (indented_code_block) @md-ts-mode--fontify-indented-block))
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
       (link_text) :? @text
       (:match "^\\^" @text))
      @md-ts-mode--fontify-label)

    :language 'markdown-inline
    :feature 'emphasis
    :override 'append
    '((emphasis) @italic
      (strong_emphasis) @bold
      (strikethrough) @md-ts-strikethrough)

    :language 'markdown-inline
    :feature 'code_inline
    :override t
    '((code_span
       :anchor (code_span_delimiter) @open @md-ts-delimiter
       (:equal "`" @open)
       (code_span_delimiter) @close @md-ts-delimiter
       (:equal "`" @close) :anchor)
      @md-ts-code-inline)

    :language 'markdown-inline
    :feature 'delimiter
    :override t
    '((emphasis_delimiter) @md-ts-delimiter
      ;; (hard_line_break) @md-ts-line-break
      )

    :language 'markdown-inline
    :feature 'reference
    '([(entity_reference)
       (numeric_character_reference)]
      @md-ts-reference)

    :language 'markdown-inline
    :feature 'link
    '((inline_link
       (link_text) :? @md-ts-link-text
       (link_destination) :? @md-ts-link-url
       (link_title) :? @md-ts-link-title)
      (inline_link ["[" "]" "(" ")"] @md-ts-delimiter)

      (full_reference_link
       (link_text) :? @md-ts-link-text
       (link_label) :? @md-ts-mode--fontify-label)
      (full_reference_link ["[" "]"] @md-ts-delimiter)

      (collapsed_reference_link
       (link_text) :? @md-ts-link-text)
      (collapsed_reference_link ["[" "]"] @md-ts-delimiter)

      [(uri_autolink) (email_autolink)] @md-ts-link-url)

    :language 'markdown-inline
    :feature 'image
    '((image
       "!" @md-ts-image-marker
       (image_description) :? @md-ts-link-text
       (link_destination) :? @md-ts-link-url
       (link_title) :? @md-ts-link-title
       (link_label) :? @md-ts-mode--fontify-label)
      (image ["[" "]" "(" ")"] @md-ts-delimiter)))

   (when md-ts-mode-enable-extensions
     (treesit-font-lock-rules
      :language 'markdown-inline
      :feature 'wiki_link
      '((wiki_link (link_destination) :? @md-ts-link-url
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
     ;; FIXME failed end_tag
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

(defvar md-ts-mode--embedded-range-rules
  '((:embed 'markdown-inline
     :host 'markdown
     ;; NOTE use local parser to parse code_span precisely
     :local t
     '((inline) @cap
       (pipe_table_row (pipe_table_cell) @cap)))

    (:embed 'html
     :host 'markdown
     '((html_block) @cap))

    ;; FIXME failed to capture html in inline sometimes
    (:embed 'html
     :host 'markdown-inline
     :local t
     '((html_tag) @cap))

    (:embed 'yaml
     :host 'markdown
     '((minus_metadata) @cap))

    (:embed 'toml
     :host 'markdown
     '((plus_metadata) @cap))))

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

(defun md-ts-mode--markdown-inline-p (point)
  "Return non-nil if language at POINT should be markdown-inline."
  (when-let* ((node (treesit-node-at point 'markdown))
              (type (treesit-node-type node)))
    (or (string= type "inline")
        (and (string= type "pipe_table_cell")
             (string= (treesit-node-type (treesit-node-parent node))
                      "pipe_table_row")))))

(defmacro md-ts-mode--regexp-opt (&rest body)
  "Return optional regex string based on BODY."
  (declare (indent 1) (debug t))
  `(regexp-opt (delq nil (list ,@body))))

(defun md-ts-mode--enabled-feature ()
  "Return a list of features which are enabled in `md-ts-mode'."
  (seq-keep (lambda (s)
              (and (treesit-font-lock-setting-enable s)
                   (treesit-font-lock-setting-feature s)))
            treesit-font-lock-settings))

(defun md-ts-mode--language-at-point (point)
  "Return the language at POINT for `md-ts-mode'."
  (if-let* ((node (treesit-node-at point 'markdown)))
      (pcase (treesit-node-type node)
        ("minus_metadata" 'yaml)
        ("plus_metadata" 'toml)
        ("html_block" 'html)
        ((guard (md-ts-mode--markdown-inline-p point))
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

(defun md-ts-mode--fontify-label (node override start end &rest _)
  "Function to fontify matched link label or footnote label.
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

;; HACK remove trailing newlines for (indented_code_block)
(defun md-ts-mode--fontify-indented-block (node override start end &rest _)
  "Fontify an indented_code_block NODE, excluding trailing newlines.
NODE, OVERRIDE, START, END refer to `font-lock-keywords' documentation."
  (let* ((n-start (treesit-node-start node))
         (n-end-raw (treesit-node-end node))
         (n-end (save-excursion
                  (goto-char n-end-raw)
                  ;; Skip all trailing newlines
                  (while (and (> (point) n-start)
                              (eq (char-before) ?\n))
                    (backward-char 1))
                  ;; Move forward to include one newline, if possible
                  (when (< (point) n-end-raw)
                    (forward-char 1))
                  (point))))
    (treesit-fontify-with-override n-start n-end 'md-ts-indented-block
                                   override start end)))


;; fontify fenced_code_block natively

(defvar md-ts-mode--language-cache nil)

(defun md-ts-mode--get-fenced-language (node)
  "Return the language info of fenced code block NODE located."
  (thread-first
    node
    (treesit-node-parent)
    (treesit-filter-child
     (lambda (n) (string= (treesit-node-type n) "info_string")) t)
    (car)
    (treesit-filter-child
     (lambda (n) (string= (treesit-node-type n) "language")) t)
    (car)
    (treesit-node-text t)
    (downcase)))

(defun md-ts-mode--get-fenced-fallback-mode ()
  "Return fallback major mode if fenced code block failed to handle."
  (if-let* ((fallback (cdr (assoc "" md-ts-mode--language-cache))))
      fallback
    (let ((new (or (cadr (assq nil md-ts-mode-language-mappings)) 'text-mode)))
      (push (cons "" new) md-ts-mode--language-cache)
      new)))

(defun md-ts-mode--get-fenced-major-mode (language)
  "Return the major mode of fenced code block LANGUAGE alias refer to."
  (unless (assoc language md-ts-mode--language-cache)
    (let* ((mode (thread-last
                   md-ts-mode-language-mappings
                   (seq-find (lambda (x)
                               (member language (mapcar #'downcase (car x)))))
                   (cdr)
                   (seq-find #'fboundp))))
      (push (cons language (or mode (md-ts-mode--get-fenced-fallback-mode)))
            md-ts-mode--language-cache)))
  (cdr (assoc language md-ts-mode--language-cache)))

;; Based on `markdown-fontify-code-block-natively' from markdown-mode
(defun md-ts-mode--fontify-fenced-block (node &rest _)
  "Fontify a fenced code block NODE using native mode highlighting.
Called during font-lock when `md-ts-mode-fontify-fenced-blocks-natively'
is non-nil.  NODE is a Tree-sitter node representing a fenced code block (e.g.,
GitHub Flavored Markdown).  The function extracts the language from NODE,
applies the corresponding major mode’s font-lock rules, and highlights the block
from its start to end positions in the current buffer.  Additional arguments
beyond NODE are ignored.  The block’s text is processed in a temporary buffer,
and its faces are transferred back, with `md-ts-code-block' appended as a
default face. Text properties are set to ensure proper fontification and buffer
state is preserved."
  (when md-ts-mode-fontify-fenced-blocks-natively
    (let* ((lang (md-ts-mode--get-fenced-language node))
           (mode (md-ts-mode--get-fenced-major-mode lang))
           (start (treesit-node-start node))
           (end (treesit-node-end node))
           (text (treesit-node-text node t))
           (modified (buffer-modified-p))
           (md-buffer (current-buffer))
           pos next)
      ;; remove all face value
      (remove-text-properties start end '(face nil))
      (with-current-buffer
          (get-buffer-create
           (format " *md-ts-mode-fenced-buffer-%s*" (symbol-name mode)))
        (let ((inhibit-modification-hooks nil))
          (delete-region (point-min) (point-max))
          (insert text " ")) ;; so there's a final property change
        (condition-case nil
            (unless (eq major-mode mode)
              (funcall mode))
          ;; use fallback instead if funcall major mode failed.
          (error
           (let ((fallback (md-ts-mode--get-fenced-fallback-mode)))
             (message "md: error happend when funcall %S, call %S instead."
                      mode fallback)
             (funcall fallback)
             (setf (alist-get lang md-ts-mode-language-mappings nil nil #'string=)
                   fallback))))
        (font-lock-ensure)
        (setq pos (point-min))
        (while (setq next (next-single-property-change pos 'face))
          (let ((val (get-text-property pos 'face)))
            (when val
              (put-text-property
               (+ start (1- pos)) (1- (+ start next)) 'face
               val md-buffer)))
          (setq pos next)))
      ;; append default code block face
      (font-lock-append-text-property start end 'face 'md-ts-code-block md-buffer)
      (add-text-properties
       start end
       '(font-lock-fontified t fontified t font-lock-multiline t))
      (set-buffer-modified-p modified))))

(defun md-ts-mode--fontify-region-hybridly (start end &optional loudly)
  "Fontify markdown buffer with both tree-sitter and traditional keywords.
START, END, LOUDLY is same with"
  (treesit-font-lock-fontify-region start end loudly)
  (font-lock-fontify-keywords-region start end loudly))

;; TODO font-lock-keywords

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


;; Navigation

(defun md-ts-mode--defun-name (node)
  "Return heading NODE name for `md-ts-mode'."
  (when-let* (((string-match-p treesit-defun-type-regexp (treesit-node-type node)))
              (target (treesit-node-child-by-field-name node "heading_content")))
    (string-trim (treesit-node-text target))))

(defvar md-ts-mode--thing-settings
  `((markdown
     (sentence ,(rx (or "paragraph" "list" "pipe_table" "fenced_code_block"
                        "indented_code_block")))
     (list ,(rx (or  "pipe_table_row" "pipe_table_header"
                     "pipe_table_delimiter_row")))
     (sexp ,(rx (or "list_item"))))
    (markdown-inline
     (sexp ,(rx (or "inline_link" "full_reference_link" "collapsed_reference_link"
                    "uri_autolink" "email_autolink" "shortcut_link" "image"
                    "code_span"))))
    (html
     (sexp ,(regexp-opt '("element" "text" "attribute" "value")))
     (list ,(rx (or "element" "comment")))
     (sentence ,(rx (and bos (or "tag_name" "attribute") eos)))
     (text ,(regexp-opt '("comment" "text"))))
    (toml
     (list
      ,(rx bos (or "array" "inline_table") eos))
     (sentence
      ,(rx bos (or "pair") eos))
     (text
      ,(rx bos (or "comment") eos)))
    (yaml
     (list ,(rx (or "block_mapping_pair" "flow_sequence")))
     (sentence ,"block_mapping_pair"))))


;; Imenu

(defun md-ts-mode--heading-name (node)
  "Return the text content of the heading NODE's inline content."
  (when-let* ((parent (treesit-node-parent node)))
    (md-ts-mode--defun-name parent)))

(defun md-ts-mode--imenu-headings ()
  "Return a list of Markdown heading rules for imenu integration.
Each rule is a list (NAME PATTERN nil EXTRACTOR) for ATX headings (H1-H6).
NAME is the heading level (e.g., \"H1\"), PATTERN matches Tree-sitter
node types like \"atx_h1_marker\", and EXTRACTOR is `md-ts-heading-name'."
  (let ((heading-levels (number-sequence 1 6))
        (type-str (md-ts-mode--regexp-opt
                   "atx_h%d_marker"
                   (when md-ts-mode-enable-setext-heading
                     "setext_h%d_underline")))
        rules)
    (dolist (level heading-levels rules)
      (push (list (format "H%d" level)
                  (format (concat "\\`" type-str "\\'") level level)
                  nil
                  #'md-ts-mode--heading-name)
            rules))
    (nreverse rules)))


;; Dnd

;; Based on `markdown--dnd-local-file-handler' from markdown-mode
(defun md-ts-mode--dnd-file-handler (urls _action)
  "Insert links with given URLS in `md-ts-mode'."
  (let ((index 0)
        (pos (point)))
    (dolist (url urls)
      (setq index (1+ index))
      (let* ((filename (dnd-get-local-file-name url))
             (mimetype (mailcap-file-name-to-mime-type filename))
             (text (format "%d-link" index))
             (destination (file-relative-name filename))
             (prefix (if (string-prefix-p "image/" mimetype) "!" " ")))
        (when (string-match-p "\\s-" destination)
          (setq destination (concat "<" destination ">")))
        (insert (format "%s[%s](%s)" prefix text destination)))
      (when (> (length urls) 1) (insert "\n")))
    (goto-char (+ pos 8))))

(put 'md-ts-mode--dnd-multi-local-file-handler 'dnd-multiple-handler t)


;; Major mode

;;;###autoload
(define-derived-mode md-ts-mode text-mode "MD"
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

  ;; Navigation
  (setq-local treesit-defun-type-regexp
              (md-ts-mode--regexp-opt
               "atx_heading"
               (when md-ts-mode-enable-setext-heading
                 "setext_heading")))
  (setq-local treesit-defun-name-function #'md-ts-mode--defun-name)
  ;; BUG predicate error
  ;; (setq-local treesit-thing-settings md-ts-mode--thing-settings)

  ;; Imenu
  (setq-local treesit-simple-imenu-settings `(,@(md-ts-mode--imenu-headings)))

  ;; dnd
  (let ((dnd-handler #'md-ts-mode--dnd-file-handler))
    (setq-local dnd-protocol-alist
                (append
                 `(("^file:///"   . ,dnd-handler)
                   ("^file:/[^/]" . ,dnd-handler)
                   ("^file:[^/]"  . ,dnd-handler))
                 dnd-protocol-alist)))

  (treesit-major-mode-setup))

(derived-mode-add-parents 'md-ts-mode '(markdown-mode))

(when (treesit-ready-p 'markdown 'quiet)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . md-ts-mode)))

(provide 'md-ts-mode)
;;; md-ts-mode.el ends here
