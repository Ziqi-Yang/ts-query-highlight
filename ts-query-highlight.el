;;; ts-query-highlight.el --- Interactively highlight tree sitter queries -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Ziqi Yang <mr.meowking@anche.no>

;; Version: 0.1.0
;; Author: Ziqi Yang <mr.meowking@anche.no>
;; Keywords: convenience, tools, languages
;; URL: https://git.sr.ht/~meow_king/ts-query-highlight
;; License: GNU General Public License >= 3
;; Package-Requires: ((emacs "29.1"))

;; This file is NOT part of Emacs.
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package can interactively highlight your tree sitter queries.  It is
;; useful when you want to develop a new tree sitter major mode.

;; Colorscheme: https://github.com/catppuccin/catppuccin
;;  dark: Latte
;;  light: Frappé
;; TODO light

;;; Code:

(require 'treesit)

(defgroup ts-query-highlight nil
  "Tree sitter query highlight."
  :group 'conventience)

(defface ts-query-highlight-face-1
  '((((class color) (background dark))
     :background "#dc8a78")
    (((class color) (background light))
     :background "#f2d5cf"))
  "Background highlight face for first caught query."
  :group 'ts-query-highlight)

(defface ts-query-highlight-face-2
  '((((class color) (background dark))
     :background "#ea76cb")
    (((class color) (background light))
     :background "#f4b8e4"))
  "Background highlight face for first caught query."
  :group 'ts-query-highlight)

(defface ts-query-highlight-face-3
  '((((class color) (background dark))
     :background "#8839ef")
    (((class color) (background light))
     :background "#ca9ee6"))
  "Background highlight face for first caught query."
  :group 'ts-query-highlight)

(defface ts-query-highlight-face-4
  '((((class color) (background dark))
     :background "#d20f39")
    (((class color) (background light))
     :background "#e78284"))
  "Background highlight face for first caught query."
  :group 'ts-query-highlight)

(defface ts-query-highlight-face-5
  '((((class color) (background dark))
     :background "#df8e1d")
    (((class color) (background light))
     :background "#ef9f76"))
  "Background highlight face for first caught query."
  :group 'ts-query-highlight)


(defface ts-query-highlight-face-6
  '((((class color) (background dark))
     :background "#40a02b")
    (((class color) (background light))
     :background "#e5c890"))
  "Background highlight face for first caught query."
  :group 'ts-query-highlight)

(defface ts-query-highlight-face-7
  '((((class color) (background dark))
     :background "#04a5e5")
    (((class color) (background light))
     :background "#a6d189"))
  "Background highlight face for first caught query."
  :group 'ts-query-highlight)

(defface ts-query-highlight-face-8
  '((((class color) (background dark))
     :background "#209fb5")
    (((class color) (background light))
     :background "#81c8be"))
  "Background highlight face for first caught query."
  :group 'ts-query-highlight)

(defface ts-query-highlight-face-9
  '((((class color) (background dark))
     :background "#1e66f5")
    (((class color) (background light))
     :background "#8caaee"))
  "Background highlight face for first caught query."
  :group 'ts-query-highlight)

(defvar ts-query-highlight-faces
  '(ts-query-highlight-face-1
    ts-query-highlight-face-2
    ts-query-highlight-face-3
    ts-query-highlight-face-4
    ts-query-highlight-face-5
    ts-query-highlight-face-6
    ts-query-highlight-face-7
    ts-query-highlight-face-8
    ts-query-highlight-face-9)
  "The faces used to highlight queries.")

(defun ts-query-highlight--validate-buffer ()
  "To examine whether the current buffer has a tree sitter parser.
Throw an error if tree sitter is not available or there is no parser in the
current buffer.
Return t."
  (unless (treesit-available-p)
    (error "Tree Sitter not available!"))
  (unless (treesit-parser-list)
    (error "There is no parser in the current buffer!"))
  t)

(defun ts-query-highlight--create-highlight-alist (query &optional face-num)
  "Create capture name -> highlight face alist.
Capture names are typically the symbol after `@' in QUERY.
FACE-NUM: the nth face in the `ts-query-highlight-faces'.
Highlighting order:
 ((((_) @c) @b (_) @d) @a ((_) @h) (((_) @g) @f) @e)"
  ;; keys: symbol name after '@' (string)
  (let ((res '())
        (face-num (if face-num face-num 0))
        (lists '())  ;; lists in current layer for recursive call
        return-list) ;; list returned by recursive call
    ;; don't use `(flatten-list query)', so user can get predictable highlighting
    (dolist (sym query)
      (if (listp sym)
          (setq lists (append lists (list sym)))
        (when (string-match "@[^z-a]+" (symbol-name sym))
          (setq res (append
                     res (list
                          (cons (substring (symbol-name sym) 1)
                                (nth face-num ts-query-highlight-faces)))))
          (setq face-num (1+ face-num)))
        (dolist (l (reverse lists))
          (setq return-list (ts-query-highlight--create-highlight-alist l face-num))
          (setq face-num (+ (length return-list) face-num))
          (setq res (append res return-list)))
        (setq lists nil)))
    res))

(defun ts-query-highlight-execute (query)
  "Execute and highlight the QUERY result."
  (interactive (list (when (ts-query-highlight--validate-buffer)
                       (read--expression "Query: "))))
  (let ((result (treesit-query-capture (treesit-buffer-root-node) query))
        (highlight-alist (ts-query-highlight--create-highlight-alist query))
        key node face ol)
    (ts-query-highlight-clean (point-min) (point-max))
    (dolist (entry result)
      (setq key (car entry)
            node (cdr entry))
      (setq face (cdr (assoc-string key highlight-alist))
            ol (make-overlay (treesit-node-start node) (treesit-node-end node)))
      (overlay-put ol 'id 'ts-query-highlight) ;; custom property, for easier cleaning
      (overlay-put ol 'face face)
      (overlay-put ol 'help-echo (symbol-name key)))))

(defun ts-query-highlight-clean (begin end)
  "Clean ts-query-highlight faces between region BEGIN and END.
When in interactive use, then the region is the whole buffer."
  (interactive (list (point-min) (point-max)))
  (remove-overlays begin end 'id 'ts-query-highlight))

(provide 'ts-query-highlight)

;;; ts-query-highlight.el ends here
