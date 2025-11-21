;;; "Compiled" snippets and support files for `zig-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'zig-mode
                     '(("union" "union ${1:(enum)} {\n    $0\n}" "union statement" nil nil nil "/Users/yojan/.emacs.d/snippets/zig-mode/union" nil nil)
                       ("test" "test \"$1\" {\n    $0\n}" "test statement" nil nil nil "/Users/yojan/.emacs.d/snippets/zig-mode/test" nil nil)
                       ("switch" "switch ($1) {\n    $2 => $0\n}" "switch statement" nil nil nil "/Users/yojan/.emacs.d/snippets/zig-mode/switch" nil nil)
                       ("struct" "struct {\n    $0\n}" "struct declaration" nil nil nil "/Users/yojan/.emacs.d/snippets/zig-mode/struct" nil nil)
                       ("main" "pub fn main($1) ${2:${!void}} {\n    $0\n}" "main pub function" nil nil nil "/Users/yojan/.emacs.d/snippets/zig-mode/main" nil nil)
                       ("imp" "const ${1:name} = @import(\"${2:lib}\");" "import statement" nil nil nil "/Users/yojan/.emacs.d/snippets/zig-mode/imp" nil nil)
                       ("if" "if ($1) {\n    $0\n}\n" "if statement" nil nil nil "/Users/yojan/.emacs.d/snippets/zig-mode/if" nil nil)
                       ("for" "for ($1) ${2:|${3:value}|} {\n    $0\n}" "for statement" nil nil nil "/Users/yojan/.emacs.d/snippets/zig-mode/for" nil nil)
                       ("fn" "fn ${1:name}($2) ${3:!void} {\n   $0\n}" "fn name() type { ... }" nil nil nil "/Users/yojan/.emacs.d/snippets/zig-mode/fn" nil nil)
                       ("enum" "enum${1:(${2:type})} {\n    $0\n}" "enum declaration" nil nil nil "/Users/yojan/.emacs.d/snippets/zig-mode/enum" nil nil)
                       ("el" "else {\n    $0\n}" "else sttement" nil nil nil "/Users/yojan/.emacs.d/snippets/zig-mode/else" nil nil)
                       ("elif" "else if (${1}) {\n     ${0}\n}" "else if statement" nil nil nil "/Users/yojan/.emacs.d/snippets/zig-mode/elif" nil nil)))


;;; Do not edit! File generated at Thu Nov 20 13:20:18 2025
