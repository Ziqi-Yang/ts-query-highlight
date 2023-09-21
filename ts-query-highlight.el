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

;; If you want to set layout, custom `ts-query-highlight-panel-display-buffer-parameters'
;; and set display-buffer-alist for `treesit-explore-mode' like
;; (add-to-list 'display-buffer-alist
;;              '("^\\*tree-sitter explorer for [^z-a]+\\*"
;;                display-buffer-in-side-window
;;                (side . right)
;;                (window-width . 70)))

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

(defcustom ts-query-highlight-panel-auto-verify-query-when-error t
  "Auto use `treesit-query-verify' when executing query resulting in error."
  :type 'boolean
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

(defface ts-query-highlight-mismatch-capture-face
  '((t (:underline (:style wave :color "#d20f39"))))
  "Face for mismatch capture face."
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

(defun ts-query-highlight-mark-highlight ()
  "Mark current highlight region at point."
  (interactive)
  (let* ((ol (car (overlays-at (point) t)))
         (capture (overlay-get ol 'help-echo)))
    (push-mark (overlay-start ol))
    (goto-char (overlay-end ol))
    (activate-mark)
    (message "Current capture is: %s" capture)))

(defvar-keymap ts-query-highlight-overlay-map
  :doc "Keymap used on ts-query-highlight overlays."
  "m" #'ts-query-highlight-mark-highlight
  "RET" #'ts-query-highlight-mark-highlight)

;;;###autoload
(defun ts-query-highlight-execute (query &optional keys)
  "Execute and highlight the QUERY result.
KEYS: A list of string.  if provided, then only highlight those KEYS(captures)."
  (interactive (list (when (ts-query-highlight--validate-buffer)
                       (ts-query-highlight--read-experssion (buffer-name (current-buffer)) "Query: "))))
  (let ((result (treesit-query-capture (treesit-buffer-root-node) query))
        (face-num 0)
        highlight-alist key node face ol)
    (ts-query-highlight-clean (point-min) (point-max))
    (dolist (entry result)
      (setq key (symbol-name (car entry)) ;; string version of capture
            node (cdr entry))
      (setq face (cdr (assoc-string key highlight-alist)))
      (unless face
        (setq highlight-alist (append highlight-alist
                                      (list (cons key (nth (% face-num (length ts-query-highlight-faces)) ts-query-highlight-faces)))))
        (setq face-num (1+ face-num)))
      (when (or (not keys)
                (member key keys))
        (setq face (cdr (assoc key highlight-alist)))
        (setq ol (make-overlay (treesit-node-start node) (treesit-node-end node)))
        (overlay-put ol 'id 'ts-query-highlight) ;; custom property, for easier cleaning
        (overlay-put ol 'keymap ts-query-highlight-overlay-map)
        (overlay-put ol 'face face)
        (overlay-put ol 'help-echo key)))
    (setq ts-query-highlight-alist highlight-alist)))

;;;###autoload
(defun ts-query-highlight-clean (begin end)
  "Clean ts-query-highlight faces between region BEGIN and END.
When in interactive use, then the region is the whole buffer."
  (interactive (list (point-min) (point-max)))
  (remove-overlays begin end 'id 'ts-query-highlight))

(defun ts-query-highlight-panel--get-ts-explore-buffer-name (target-buffer-name)
  "Get `treesit-explore-mode' buffer name.  Based on TARGET-BUFFER-NAME."
  (format "*tree-sitter explorer for %s*" target-buffer-name))

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
    (unless (get-buffer (ts-query-highlight-panel--get-ts-explore-buffer-name
                         ts-query-highlight-target-buffer-name))
      (treesit-explore-mode))
    (with-current-buffer panel-buffer
      (erase-buffer)
      (insert "(())")
      (backward-char 2)
      (ts-query-highlight-panel-mode)
      (setq-local ts-query-highlight-target-buffer-name (buffer-name target-buffer)))
    (pop-to-buffer panel-buffer
                   ts-query-highlight-panel-display-buffer-parameters)))

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

(defun ts-query-highlight-panel-overlay-send-query ()
  "Only highlight current capture (capture at point)."
  (interactive)
  (let* ((ol (car (overlays-at (point) t)))
         (cur-capture (buffer-substring (overlay-start ol) (overlay-end ol))))
    (ts-query-highlight-panel-mode-send-query (list cur-capture))))

(defvar-keymap ts-query-highlight-panel-overlay-map
  :doc "Keymap used on ts-query-highlight overlays."
  "RET" #'ts-query-highlight-panel-overlay-send-query)

(defun ts-query-highlight-panel-mode-send-query (&optional keys)
  "Send text inside query panel as query and highlight the result.
KEYS: keys for `ts-query-highlight-execute'."
  (interactive)
  (condition-case nil
      (let* ((text (buffer-string))
             (query (read text))
             alist)
        (with-current-buffer ts-query-highlight-target-buffer-name
          (ts-query-highlight-execute query keys)
          (setq alist ts-query-highlight-alist))
        (remove-overlays)
        (save-excursion
          ;; (rx "@" (1+ (or (syntax word) (syntax symbol))))
          ;; "@\\(?:\\sw\\|\\s_\\)+"
          ;; make sure we use Emacs lisp mode syntax table
          (goto-char (point-min))
          (while (search-forward-regexp "@\\(?:\\sw\\|\\s_\\)+" nil t)
            (let* ((start (save-excursion (1+ (search-backward "@"))))
                   (end (point))
                   (ol (make-overlay start end))
                   (key (buffer-substring start end))
                   (face (cdr (assoc-string key alist)))
                   (face (if face ;; never meet this condition?
                             face
                           'ts-query-highlight-mismatch-capture-face)))
              (overlay-put ol 'id 'ts-query-highlight) ;; custom property, for easier cleaning
              (overlay-put ol 'keymap ts-query-highlight-panel-overlay-map)
              (overlay-put ol 'face face)))))
    (error (ts-query-highlight-panel-valide-query))))

(defun ts-query-highlight-panel-mode-clean-query-res ()
  "Clean the query result using `ts-query-highlight-clean'."
  (interactive)
  (with-current-buffer ts-query-highlight-target-buffer-name
    (ts-query-highlight-clean (point-min) (point-max)))
  (remove-overlays (point-min) (point-max) 'id 'ts-query-highlight))

(defun ts-query-highlight-panel-mode-toggle-ts-explore ()
  "Toggle the ts-explore window."
  (interactive)
  (let* ((ts-explore-buffer-name (ts-query-highlight-panel--get-ts-explore-buffer-name
                                  ts-query-highlight-target-buffer-name))
         (ts-explore-buffer (get-buffer ts-explore-buffer-name))
         (ts-explore-window (get-buffer-window ts-explore-buffer)))
    (if ts-explore-window
        (delete-window ts-explore-window)
      (if ts-explore-buffer
          (display-buffer ts-explore-buffer)
        (with-current-buffer ts-query-highlight-target-buffer-name
          (treesit-explore-mode))))))

(defun ts-query-highlight-panel-valide-query ()
  "Use `ts-query-validate' to validate the query.
QUERY: query."
  (interactive)
  (let ((query (read (buffer-string)))
        language)
    (with-current-buffer ts-query-highlight-target-buffer-name
      (setq language (treesit-node-language (treesit-buffer-root-node))))
    (treesit-query-validate language query)))

(defun ts-query-highlight-panel-quit ()
  "Exit panel."
  (interactive)
  (with-current-buffer ts-query-highlight-target-buffer-name
    (ts-query-highlight-clean (point-min) (point-max))
    (treesit-explore-mode -1))
  (kill-buffer-and-window))

(defvar ts-query-highlight-panel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'ts-query-highlight-panel-mode-send-query)
    (define-key map (kbd "C-c C-k") #'ts-query-highlight-panel-mode-clean-query-res)
    (define-key map (kbd "C-c C-v") #'ts-query-highlight-panel-valide-query)
    (define-key map (kbd "C-c C-e") #'ts-query-highlight-panel-mode-toggle-ts-explore)
    (define-key map (kbd "C-c C-q") #'ts-query-highlight-panel-quit)
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
