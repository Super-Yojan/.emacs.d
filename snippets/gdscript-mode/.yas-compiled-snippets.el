;;; "Compiled" snippets and support files for `gdscript-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'gdscript-mode
                     '(("wh" "while ${1:True}:\n    ${0:pass}" "while" nil
                        ("control structure")
                        nil "/Users/yojan/.emacs.d/snippets/gdscript-mode/while" nil nil)
                       ("var" "var ${1:name} = $0" "var" nil
                        ("definitions")
                        nil "/Users/yojan/.emacs.d/snippets/gdscript-mode/var" nil nil)
                       ("sf" "static func ${1:func_name} (${2:param}):\n    ${0:pass}" "static_func" nil
                        ("definitions")
                        nil "/Users/yojan/.emacs.d/snippets/gdscript-mode/static_func" nil nil)
                       ("vsg" "var ${1:variable} = ${2:value} setget ${3:setterfunc}, ${4:getterfunc}" "setget" nil
                        ("definitions")
                        nil "/Users/yojan/.emacs.d/snippets/gdscript-mode/setget" nil nil)
                       ("r" "return $0" "return" nil nil nil "/Users/yojan/.emacs.d/snippets/gdscript-mode/return" nil nil)
                       ("p" "print($0)" "print" nil nil nil "/Users/yojan/.emacs.d/snippets/gdscript-mode/print" nil nil)
                       ("onr" "onready var ${1:name} = $0" "onready" nil
                        ("definitions")
                        nil "/Users/yojan/.emacs.d/snippets/gdscript-mode/onready" nil nil)
                       ("match" "match ${1:expression}:\n    ${2:pattern}:\n        ${3:pass}" "match" nil
                        ("control structure")
                        nil "/Users/yojan/.emacs.d/snippets/gdscript-mode/match" nil nil)
                       ("ife" "if ${1:condition}:\n    ${2:pass}\nelse:\n    ${3:pass}" "ife" nil
                        ("control structure")
                        nil "/Users/yojan/.emacs.d/snippets/gdscript-mode/ife" nil nil)
                       ("if" "if ${1:condition}:\n    ${0:pass}" "if" nil
                        ("control structure")
                        nil "/Users/yojan/.emacs.d/snippets/gdscript-mode/if" nil nil)
                       ("f" "func ${1:func_name} (${2:param}):\n    ${0:pass}" "func" nil
                        ("definitions")
                        nil "/Users/yojan/.emacs.d/snippets/gdscript-mode/func" nil nil)
                       ("for" "for ${var} in ${collection}:\n    ${0:pass}" "for ... in ... : ..." nil
                        ("control structure")
                        nil "/Users/yojan/.emacs.d/snippets/gdscript-mode/for" nil nil)
                       ("enum" "enum $1 {$2}" "enum" nil
                        ("definitions")
                        nil "/Users/yojan/.emacs.d/snippets/gdscript-mode/enum" nil nil)
                       ("const" "const ${1:name} = $0" "const" nil
                        ("definitions")
                        nil "/Users/yojan/.emacs.d/snippets/gdscript-mode/const" nil nil)
                       ("clsn" "class_name ${1:Item} = ${2:\"res://icons/item.png\"}" "class_name" nil
                        ("object oriented")
                        nil "/Users/yojan/.emacs.d/snippets/gdscript-mode/class_name" nil nil)
                       ("cls" "class ${1:class}:\n    $0\n" "class" nil
                        ("object oriented")
                        nil "/Users/yojan/.emacs.d/snippets/gdscript-mode/class" nil nil)))


;;; Do not edit! File generated at Thu Nov 20 13:20:18 2025
