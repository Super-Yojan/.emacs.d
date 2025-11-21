;;; "Compiled" snippets and support files for `racket-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'racket-mode
                     '(("when" "(when ${1:(${2:predicate})} $0)" "(when ...)" nil nil nil "/Users/yojan/.emacs.d/snippets/racket-mode/when" nil nil)
                       ("unless" "(unless ${1:(${2:predicate})} $0)" "(unless ...)" nil nil nil "/Users/yojan/.emacs.d/snippets/racket-mode/unless" nil nil)
                       ("syntax-rules" "(syntax-rules (${1:literal-id}$2)\n  [(${3:id} ${4:pattern}) ${5:template}]$6)" "(syntax-rules (...) [(... ...) ...]...)" nil nil nil "/Users/yojan/.emacs.d/snippets/racket-mode/syntax-rules" nil nil)
                       ("module*" "(module* ${1:id} ${2:module-path}\n  $0)" "(module* ... ... ...)" nil nil nil "/Users/yojan/.emacs.d/snippets/racket-mode/module-star" nil nil)
                       ("module+" "(module+ ${1:id}\n  $0)" "(module+ ... ...)" nil nil nil "/Users/yojan/.emacs.d/snippets/racket-mode/module-plus" nil nil)
                       ("module" "(module ${1:id} ${2:module-path}\n  $0)" "(module ... ... ...)" nil nil nil "/Users/yojan/.emacs.d/snippets/racket-mode/module" nil nil)
                       ("match" "(match ${1:expression} [${2:clause} $0])" "(match ... [... ...]...)" nil nil nil "/Users/yojan/.emacs.d/snippets/racket-mode/match" nil nil)
                       ("let" "(let$1 ([${2:name} ${3:expression}]$4) $0)" "(let... ([... ...]...) ...)" nil nil nil "/Users/yojan/.emacs.d/snippets/racket-mode/let" nil nil)
                       ("lambda" "(lambda ${1:(${2:arguments})} $0)" "(lambda (...) ...)" nil nil nil "/Users/yojan/.emacs.d/snippets/racket-mode/lambda" nil nil)
                       ("if" "(if ${1:(${2:predicate})}\n    $0)" "(if ... ... ...)" nil nil nil "/Users/yojan/.emacs.d/snippets/racket-mode/if" nil nil)
                       ("for" "(for$1 (${2:for-clause}) $0)" "(for... (...) ...)" nil nil nil "/Users/yojan/.emacs.d/snippets/racket-mode/for" nil nil)
                       ("do" "(do ([${1:name} ${2:init} ${3:step}]$4)\n    (${5:stop-predicate} ${6:finish})\n  $0)" "(do ([... ... ...]...) (... ...) ...)" nil nil nil "/Users/yojan/.emacs.d/snippets/racket-mode/do" nil nil)
                       ("define-syntax-rule" "(define-syntax-rule (${1:id} ${2:pattern}) $0)" "(define-syntax-rule (... ...) ...)" nil nil nil "/Users/yojan/.emacs.d/snippets/racket-mode/define-syntax-rule" nil nil)
                       ("define" "(define ${1:(${2:name} ${3:arguments})} $0)" "(define ... ...)" nil nil nil "/Users/yojan/.emacs.d/snippets/racket-mode/define" nil nil)
                       ("cond" "(cond [${1:predicate} $0])" "(cond [... ...]...)" nil nil nil "/Users/yojan/.emacs.d/snippets/racket-mode/cond" nil nil)
                       ("case-lambda" "(case-lambda [${1:arguments} $0])" "(case-lambda [... ...]...)" nil nil nil "/Users/yojan/.emacs.d/snippets/racket-mode/caselambda" nil nil)
                       ("case" "(case ${1:expression} [${2:datum} $0])" "(case ... [... ...]...)" nil nil nil "/Users/yojan/.emacs.d/snippets/racket-mode/case" nil nil)))


;;; Do not edit! File generated at Thu Nov 20 13:20:18 2025
