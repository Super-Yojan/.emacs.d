;;; "Compiled" snippets and support files for `nim-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'nim-mode
                     '(("while" "while ${1:expression}:\n  $0" "while" nil nil nil "/Users/yojan/.emacs.d/snippets/nim-mode/while" nil nil)
                       ("try" "try:\n  $0\nexcept ${1:exception}:\n  " "try" nil nil nil "/Users/yojan/.emacs.d/snippets/nim-mode/try-except" nil nil)
                       ("template" "template ${1:name}(${2:arguments}): ${3:return type} =\n  $0" "template" nil nil nil "/Users/yojan/.emacs.d/snippets/nim-mode/template" nil nil)
                       ("seq" "seq[${1:type}]$0" "seq" nil nil nil "/Users/yojan/.emacs.d/snippets/nim-mode/sequence" nil nil)
                       ("proc" "proc ${1:name}(${2:arguments}): ${3:return type} =\n  $0" "proc" nil nil nil "/Users/yojan/.emacs.d/snippets/nim-mode/procedure" nil nil)
                       ("pr" "{.${1:name}.}" "pr" nil nil nil "/Users/yojan/.emacs.d/snippets/nim-mode/pragma" nil nil)
                       ("of" "of ${1:value}:\n  $0" "of" nil nil nil "/Users/yojan/.emacs.d/snippets/nim-mode/of" nil nil)
                       ("method" "method ${1:name}(${2:arguments}): ${3:return type} =\n  $0" "method" nil nil nil "/Users/yojan/.emacs.d/snippets/nim-mode/method" nil nil)
                       ("macro" "macro ${1:name}(${2:arguments}): ${3:return type} =\n  $0" "macro" nil nil nil "/Users/yojan/.emacs.d/snippets/nim-mode/macro" nil nil)
                       ("iterator" "iterator ${1:name}(${2:arguments}): ${3:return type}$0" "iterator" nil nil nil "/Users/yojan/.emacs.d/snippets/nim-mode/iterator" nil nil)
                       ("from" "from ${1:module} import ${2:field}" "from" nil nil nil "/Users/yojan/.emacs.d/snippets/nim-mode/import from" nil nil)
                       ("import" "import ${1:module}" "import" nil nil nil "/Users/yojan/.emacs.d/snippets/nim-mode/import" nil nil)
                       ("if" "if ${1:expression}:\n  $0" "if" nil nil nil "/Users/yojan/.emacs.d/snippets/nim-mode/if" nil nil)
                       ("func" "func ${1:name}(${2:arguments}): ${3:return type} =\n  $0" "func" nil nil nil "/Users/yojan/.emacs.d/snippets/nim-mode/function" nil nil)
                       ("for" "for ${1:index} in ${2:sequence}:\n  $0" "for" nil nil nil "/Users/yojan/.emacs.d/snippets/nim-mode/for" nil nil)
                       ("case" "case ${1:value}\n$0" "case" nil nil nil "/Users/yojan/.emacs.d/snippets/nim-mode/case" nil nil)
                       ("block" "block ${1:name}:\n  $0" "block" nil nil nil "/Users/yojan/.emacs.d/snippets/nim-mode/block" nil nil)
                       ("array" "array[${1:length}, ${2:type}]$0" "array" nil nil nil "/Users/yojan/.emacs.d/snippets/nim-mode/array" nil nil)))


;;; Do not edit! File generated at Thu Nov 20 13:20:18 2025
