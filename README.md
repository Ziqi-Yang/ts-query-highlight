# ts-query-highlight

![Static Badge](https://img.shields.io/badge/Made_with-Emacs-purple)

[project](https://sr.ht/~meow_king/ts-query-highlight/) / 
[mailing lists(discussions, patches, announcements)](https://sr.ht/~meow_king/ts-query-highlight/lists) / 
[tickets(todos, bugs)](https://sr.ht/~meow_king/ts-query-highlight/trackers)

This package can help you visualize tree sitter query. If you want to develop a new tree sitter major mode, you probably need it! 

This package is highly inspired by `combobulate-query-build` command in [combobulate](https://github.com/mickeynp/combobulate), and it can be seemed as a lightweight alternative.

## Demo

Query result highlight  

![](./images/demo1.webp)

Symbol Completion Support  

![](./images/demo2.webp)

## Install && Customization

``` emacs-lisp
(use-package ts-query-highlight
  :straight (:type git :host sourcehut :repo "meow_king/ts-query-highlight")
  :config
  ;; default is `dabbrev-expand`. For `cape-dabbrev`, take a look at https://github.com/minad/cape
  (setq ts-query-highlight-dabbrev-expand-function 'cape-dabbrev))
```

If you are not satisfied with the window layout, you nay want to customize `ts-query-highlight-panel-display-buffer-parameters` for the panel window, and 
customize `display-buffer-alist` for `treesit-explore-mode` like

``` emacs-lisp
;; tree sitter
(add-to-list 'display-buffer-alist
  '("^\\*tree-sitter explorer for [^z-a]+\\*"
     display-buffer-in-side-window
     (side . right)
     (window-width . 70)))
```

To disable auto invoke `treesit-query-verify` on executing a query containing error, 
set `ts-query-highlight-panel-auto-verify-query-when-error` to `nil`.

## Usage

`ts-query-highlight-panel`: open a panel for executing query.  
In panel:
 - `C-c C-c` to execute. `ts-query-highlight-panel-mode-send-query`
 - `C-c C-k` to remove the highlight. `ts-query-highlight-panel-mode-clean-query-res`
 - `C-c C-v` validate the query using `ts-query-validate`. `ts-query-highlight-panel-mode-clean-query-res`
 - `C-c C-e` to toggle display of the `treesit-explore-mode`. `ts-query-highlight-panel-mode-toggle-ts-explore`
 - `C-c C-q` quit. `ts-query-highlight-quit`
 
`ts-query-highlight-execute`: read query from minibuffer and execute it.   
`ts-query-highlight-clean`: remove the query highlight in the current buffer.  

Mouse hover on the overlays on source code: display capture name.  
Keymap for overlays on source code:
 - `m` or `RET`: mark the corresponding region.  
 
Keymap for overlays inside panel:
 - `RET`: only highlight capture at point on source code.  
 
Completion Support:
Use either `completion-at-point` or `ts-query-highlight--dabbrev-expand` command.

## Note

If you want the completion feature enabled, make sure you don't kill the buffer opened by `treesit-explore-mode`, since the keyward completion uses `dabbrev-expand` mechanism in that buffer.

