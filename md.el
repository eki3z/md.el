;;; md.el --- A markdown toolchain based on tree-sitter -*- lexical-binding: t -*-

;; Copyright (C) 2025 Eki Zhang

;; Author: Eki Zhang <liuyinz95@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.0.50"))
;; Keywords: languages tools
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

;; A package based on tree-sitter for markdown editing.

;;; Code:

(require 'md-ts-mode)
(require 'md-toc)


;; Customizations/public vars

(defgroup md nil
  "Settings for md."
  :prefix "md-"
  :group 'tools
  :group 'text)

(provide 'md)
;;; md.el ends here
