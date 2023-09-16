# ts-query-highlight

![Static Badge](https://img.shields.io/badge/Made_with-Emacs-purple)

[project](https://sr.ht/~meow_king/ts-query-highlight/) / 
[mailing lists(discussions, patches, announcements)](https://sr.ht/~meow_king/ts-query-highlight/lists) / 
[tickets(todos, bugs)](https://sr.ht/~meow_king/ts-query-highlight/trackers)

This package can help you visualize tree sitter query. If you want to develop a new tree sitter major mode, you probably need it! 

This package is highly inspired by `combobulate-query-build` command in [combobulate](https://github.com/mickeynp/combobulate), and it can be seemed as a lightweight alternative.

## Demo

Query result highlight
![./images/demo1.webp]

Symbol Completion Support
![./images/demo2.webp]

## Install && Customization

``` emacs-lisp
(use-package ts-query-highlight
  :straight (:type git :host sourcehut :repo "meow_king/ts-query-highlight")
  :config
  ;; default is `dabbrev-expand`. For `cape-dabbrev`, take a look at https://github.com/minad/cape
  (setq ts-query-highlight-dabbrev-expand-function 'cape-dabbrev))
```

If you are not satisfied with the window layout, take a look at `ts-query-highlight-panel-display-buffer-parameters`.

## Usage

`ts-query-highlight-panel`: open a panel for executing query. In the panel, you can use `C-c C-c` to execute, use `C-c C-k` to remove the highlight.  
`ts-query-highlight-execute`: read query from minibuffer and execute it.   
`ts-query-highlight-clean`: remove the highlight.  

## Note

If you want the completion feature enabled, make sure you don't kill the buffer opened by `treesit-explore-mode`, since the keyward completion uses `dabbrev-expand` mechanism in that buffer.

