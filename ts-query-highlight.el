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

;;; Code:

(require 'treesit)

(defgroup ts-query-highlight nil
  "Tree sitter query highlight."
  :group 'conventience)

(defcustom ts-query-highlight-panel-buffer-name "*TS-Query-Panel*"
  "The buffer name for `ts-query-highlight-panel'."
  :type 'string
  :group 'ts-query-highlight)

(defcustom ts-query-highlight-dabbrev-expand-function 'dabbrev-expand
  "Dabbrev expand function.  If you use cape, you can set it to cape-expand."
  :type 'symbol
  :group 'ts-query-highlight)

(defcustom ts-query-highlight-panel-display-buffer-parameters
  `(display-buffer-below-selected
    (window-height . fit-window-to-buffer)
    (window-min-height . 3)
    (inhibit-same-window . t))
  "Display buffer parameters."
  :type 'symbol
  :group 'ts-query-highlight)

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

(defvar-local ts-query-highlight-target-buffer-name nil
  "Target buffer name.")

(defvar-local ts-query-highlight-alist nil)

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
                                (nth (% face-num (length ts-query-highlight-faces)) ts-query-highlight-faces)))))
          (setq face-num (1+ face-num)))
        (dolist (l (reverse lists))
          (setq return-list (ts-query-highlight--create-highlight-alist l face-num))
          (setq face-num (+ (length return-list) face-num))
          (setq res (append res return-list)))
        (setq lists nil)))
    res))

(defvar-keymap ts-query-highlight--read-experssion-map
  :doc "Keymap used by `ts-query-highlight--read-experssion'."
  :parent read-expression-map)

(defun ts-query-highlight--read-experssion (target-buffer-name prompt)
  "Read treesit query experssion.
TARGET-BUFFER-NAME: the buffer for query.
PROMPT."
  (minibuffer-with-setup-hook
      (lambda ()
        (setq-local ts-query-highlight-target-buffer-name target-buffer-name)
        (set-syntax-table emacs-lisp-mode-syntax-table)
        (add-hook 'completion-at-point-functions
                  #'ts-query-highlight--dabbrev-expand nil t))
    (read-from-minibuffer prompt nil
                          ts-query-highlight--read-experssion-map t
                          'read-expression-history)))

;;;###autoload
(defun ts-query-highlight-execute (query)
  "Execute and highlight the QUERY result."
  (interactive (list (when (ts-query-highlight--validate-buffer)
                       (ts-query-highlight--read-experssion (buffer-name (current-buffer)) "Query: "))))
  (let ((result (treesit-query-capture (treesit-buffer-root-node) query))
        (highlight-alist (ts-query-highlight--create-highlight-alist query))
        key node face ol)
    (ts-query-highlight-clean (point-min) (point-max))
    (setq ts-query-highlight-alist highlight-alist)
    (dolist (entry result)
      (setq key (car entry)
            node (cdr entry))
      (setq face (cdr (assoc-string key highlight-alist))
            ol (make-overlay (treesit-node-start node) (treesit-node-end node)))
      (overlay-put ol 'id 'ts-query-highlight) ;; custom property, for easier cleaning
      (overlay-put ol 'face face)
      (overlay-put ol 'help-echo (symbol-name key)))))

;;;###autoload
(defun ts-query-highlight-clean (begin end)
  "Clean ts-query-highlight faces between region BEGIN and END.
When in interactive use, then the region is the whole buffer."
  (interactive (list (point-min) (point-max)))
  (remove-overlays begin end 'id 'ts-query-highlight))

;;;###autoload
(defun ts-query-highlight-panel ()
  "Open a panel for editing query."
  (interactive)
  (let ((target-buffer (current-buffer))
        (panel-buffer (get-buffer-create ts-query-highlight-panel-buffer-name)))
    (when (eq target-buffer panel-buffer)
      (user-error "This buffer cannot be use as target buffer"))
    (setq-local ts-query-highlight-target-buffer-name (buffer-name target-buffer))
    ;; enable treesit explore mode
    (unless (get-buffer (format "*tree-sitter explorer for %s*"
                                ts-query-highlight-target-buffer-name))
      (treesit-explore-mode))
    (with-current-buffer panel-buffer
      (erase-buffer)
      (insert "(())")
      (backward-char 2)
      (ts-query-highlight-panel-mode)
      (setq-local ts-query-highlight-target-buffer-name (buffer-name target-buffer)))
    (display-buffer panel-buffer
                    ts-query-highlight-panel-display-buffer-parameters)
    (select-window (get-buffer-window panel-buffer))))

(defun ts-query-highlight--dabbrev-expand  ()
  (interactive)
  (let* ((ts-explorer-buffer-name (format "*tree-sitter explorer for %s*"
                                          ts-query-highlight-target-buffer-name))
         (ts-explorer-buffer (get-buffer ts-explorer-buffer-name)))
    ;; make sure dabbrev-expand only search keywords from ts-explorer-buffer
    (setq-local dabbrev-select-buffers-function (lambda () (list ts-explorer-buffer)))
    (unless ts-explorer-buffer
      (user-error "Please enable `treesit-explore-mode' in original buffer first!"))
    (call-interactively ts-query-highlight-dabbrev-expand-function)))

(defun ts-query-highlight-panel-mode-send-query ()
  "Send text inside query panel as query and highlight the result."
  (interactive)
  (let* ((text (buffer-string))
         (query (read text))
         alist ol key face)
    (with-current-buffer ts-query-highlight-target-buffer-name
      (ts-query-highlight-execute query)
      (setq alist ts-query-highlight-alist))
    (remove-overlays)
    (save-excursion
      ;; TODO change the search method: regexp search then match
      ;; (rx "@" (1+ (or word "/" "-" "_")))
      ;; (while (search-forward-regexp "@\\(?:[[:word:]]\\|/\\|-\\|_\\)+" nil t)
      ;;   )
      (dolist (entry alist)
        (goto-char (point-min))
        (setq key (concat "@" (car entry))
              face (cdr entry))
        (search-forward key)
        (setq ol (make-overlay (- (point) (length key)) (point)))
        (overlay-put ol 'face face)))))

(defun ts-query-highlight-panel-mode-clean-query-res ()
  "Clean the query result using `ts-query-highlight-clean'."
  (interactive)
  (with-current-buffer ts-query-highlight-target-buffer-name
    (ts-query-highlight-clean (point-min) (point-max)))
  (remove-overlays))

(defvar ts-query-highlight-panel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'ts-query-highlight-panel-mode-send-query)
    (define-key map (kbd "C-c C-k") #'ts-query-highlight-panel-mode-clean-query-res)
    map))

(define-derived-mode ts-query-highlight-panel-mode prog-mode "TS-PANEL"
  :syntax-table emacs-lisp-mode-syntax-table
  :interactive nil
  :group 'ts-query-highlight
  (setq-local comment-start ";"
              comment-end "")
  (add-hook 'completion-at-point-functions
            #'ts-query-highlight--dabbrev-expand nil t))

(provide 'ts-query-highlight)

;;; ts-query-highlight.el ends here
