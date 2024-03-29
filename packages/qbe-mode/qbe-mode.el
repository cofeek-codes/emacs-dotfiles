;;; qbe-mode.el --- Major mode for editing QBE IL  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022
;;
;; Author:  Maksymilian Bentje Knust <maks@knust.one>
;; Version: 0.0.1
;; Keywords: languages
;; Homepage: https://github.com/mbknust/qbe-mode
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; A major mode for the QBE IL (see https://c9x.me/compile/).
;;
;;; Code:

(defvar qbe-mode-keywords
  '("export" "section" "type" "align" "function" "data")
  "List of QBE IL keywords")

(defvar qbe-mode-opcodes
  '("add" "sub" "div" "mul"
    "neg" "udiv" "rem" "urem"
    "or" "xor" "and"
    "sar" "shr" "shl"
    "stored" "stores" "storel"
    "storew" "storeh" "storeb"
    "loadd" "loads" "loadl"
    "loadsw" "loadsh" "loadsb"
    "loaduw" "loaduh" "loadub"
    "alloc4" "alloc8" "alloc16"
    "ceqd" "ceql" "ceqs" "ceqw"
    "cged" "cges" "cgtd" "cgts"
    "cled" "cles" "cltd" "clts"
    "cned" "cnel" "cnes" "cnew"
    "cod" "cos" "cuod" "cuos"
    "csgel" "csgew" "csgtl" "csgtw"
    "cslel" "cslew" "csltl" "csltw"
    "cugel" "cugew" "cugtl" "cugtw"
    "culel" "culew" "cultl" "cultw"
    "dtosi" "dtoui" "stosi" "stoui"
    "exts" "extsb" "extsh" "extsw"
    "extub" "extuh" "extuw"
    "sltof" "ultof" "swtof" "uwtof"
    "truncd" "cast" "copy" "phi"
    "vastart" "vaarg"
    "call" "jmp" "jnz" "ret")
  "List of QBE IL instructions")

(defvar qbe-mode-font-lock-defaults
  `((("[sd]_-?[[:digit:]]+\\(\\.[[:digit:]]+\\)?"
      . 'font-lock-constant-face)
     ("#.*$" . 'font-lock-comment-face)
     ("\\(:[[:alnum:]]+\\|\\<[wlsdbh]\\>\\)"
      . 'font-lock-type-face)
     ("[%\\$][[:alnum:]]*"
      . 'font-lock-variable-name-face)
     (,(format "\\<%s\\>" (regexp-opt qbe-mode-opcodes))
      . 'font-lock-builtin-face)
     (,(format "\\<%s\\>" (regexp-opt qbe-mode-keywords))
      . 'font-lock-keyword-face)
     ("\\(@[[:alnum:]]+\\|-?[[:digit:]]+\\)"
      . 'font-lock-constant-face))))

(defun qbe-mode-indent-line ()
 "Indent current line as QBE code."
 (interactive)
 (let ((indent-col 0))
    (save-excursion
      (beginning-of-line)
      (condition-case nil
          (while t
            (backward-up-list 1)
            (when (looking-at "\\s(")
              (setq indent-col (+ indent-col tab-width))))
        (error nil)))
    (if (eq (char-after) ?})
        (setq indent-col (- indent-col tab-width)))
    (goto-char (point-at-bol))
    (delete-region (point) (progn (skip-chars-forward " \t") (point)))
    (indent-to indent-col)))

;;;###autoload
(define-derived-mode qbe-mode prog-mode "QBE"
  "major mode for editing QBE IL"
  (setq-local font-lock-defaults qbe-mode-font-lock-defaults)
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local indent-line-function 'qbe-mode-indent-line))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ssa" . qbe-mode))

(provide 'qbe-mode)
;;; qbe-mode.el ends here
