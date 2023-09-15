;; -*- lexical-binding: t; -*-

(push (expand-file-name "/home/zarkli/proj/tree-sitter/ts-query-highlight") load-path)
(require 'ts-query-highlight)

(ts-query-highlight--create-highlight-alist
 '((function_definition type: ( (_) @e (_) @f) @c (_) @d) @a ( (_) @g (_) @h) @b))

;; C
;; '((function_definition (primitive_type) @b (pointer_declarator (function_declarator (identifier) @e (parameter_list (parameter_declaration type: (_) @h) @g ) @f) @d) @c (compound_statement) @i) @a)

(ts-query-highlight--create-highlight-alist
 '((((_) @c) @b (_) @d) @a ((_) @h) (((_) @g) @f) @e))

