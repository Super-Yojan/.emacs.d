;;; "Compiled" snippets and support files for `easycrypt-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'easycrypt-mode
                     '(("S" "search $1." "search" nil
                        ("lookup")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/lookup/search" nil nil)
                       ("P" "print $1.\n" "print" nil
                        ("lookup")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/lookup/print" nil nil)
                       ("L" "locate $1.\n" "locate" nil
                        ("lookup")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/lookup/locate" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'easycrypt-mode
                     '(("require-import" "require import $1.\n" "require-import" nil
                        ("misc")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/misc/require-import" nil nil)
                       ("range-inclusive-exclusive" "$1 <= $2 < $3" "range-inclusive-exclusive" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/misc/range-inclusive-exclusive" nil nil)
                       ("range-inclusive" "$1 <= $2 <= $3" "range-inclusive" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/misc/range-inclusive" nil nil)
                       ("range-exclusive-inclusive" "$1 < $2 <= $3" "range-exclusive-inclusive" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/misc/range-exclusive-inclusive" nil nil)
                       ("range-exclusive" "$1 < $2 < $3" "range-exclusive" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/misc/range-exclusive" nil nil)
                       ("range" "$1 <= $2 <= $3" "range" nil nil nil "/Users/yojan/.emacs.d/snippets/easycrypt-mode/misc/range" nil nil)
                       ("probability-memory" "Pr[$1 @ &$2 : $3]" "probability-memory" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/misc/probability-mem" nil nil)
                       ("probability" "Pr[$1]" "probability" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/misc/probability" nil nil)
                       ("pred" "pred $1 = $2.\n" "pred" nil
                        ("misc")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/misc/pred" nil nil)
                       ("param" "($1 : $2)" "param" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/misc/param" nil nil)
                       ("or2" "$1 || $2" "or2" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/misc/or2" nil nil)
                       ("or1" "$1 \\/ $2" "or1" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/misc/or1" nil nil)
                       ("new-comment" "(* $1 *)" "new-comment" nil nil nil "/Users/yojan/.emacs.d/snippets/easycrypt-mode/misc/new-comment" nil nil)
                       ("list" "[$1 ; $2]" "list" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/misc/list" nil nil)
                       ("let-in" "let $1 in $2" "let-in" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/misc/let-in" nil nil)
                       ("import" "import $1.\n" "import" nil
                        ("misc")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/misc/import" nil nil)
                       ("if" "if $1 then $2 else $3" "if" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/misc/if" nil nil)
                       ("hint" "hint simplify $1." "hint-simplify" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/misc/hint-simplify" nil nil)
                       ("ge0" "0 <= $1" "ge0" nil nil nil "/Users/yojan/.emacs.d/snippets/easycrypt-mode/misc/geq0" nil nil)
                       ("ge0" "0 <= $1" "ge0" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/misc/ge0" nil nil)
                       ("fun" "fun ($1) => $2" "fun" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/misc/fun" nil nil)
                       ("from-jasmin" "from Jasmin require import $1.\n" "from-jasmin" nil
                        ("misc")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/misc/from-jasmin" nil nil)
                       ("from" "from $1 require import $2.\n" "from" nil
                        ("misc")
                        nil "/Users/yojan/.emacs.d/snippets/easycrypt-mode/misc/from" nil nil)
                       ("forall" "forall ($1: $2), $3" "forall" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/misc/forall" nil nil)
                       ("comment" "(* $1 *)" "comment" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/misc/comment" nil nil)
                       ("and2" "$1 && $2" "and2" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/misc/and2" nil nil)
                       ("and1" "$1 /\\ $2" "and1" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/misc/and1" nil nil)
                       ("abbrev" "abbrev $1 = $2." "abbrev" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/misc/abbrev" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'easycrypt-mode
                     '(("module-with-return" "module $1 : $2 = {\n\n}." "module-with-return" nil
                        ("modules")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/modules/module-with-return" nil nil)
                       ("module-with-params-return" "module $1 ($2: $3) : $4 {\n\n}." "module-with-params-return" nil
                        ("modules")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/modules/module-with-params-return" nil nil)
                       ("module-with-params" "module $1 ($2: $3) : $4 {\n\n}." "module-with-params" nil nil nil "/Users/yojan/.emacs.d/snippets/easycrypt-mode/modules/module-with-params" nil nil)
                       ("module-type" "module type $1 = {\n\n}." "module-type" nil
                        ("modules")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/modules/module-type" nil nil)
                       ("module-instance" "module $1 = $2($3)." "module-instance" nil
                        ("modules")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/modules/module-instance" nil nil)
                       ("module" "module $1 = {\n\n}." "module" nil
                        ("modules")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/modules/module" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'easycrypt-mode
                     '(("while-proc" "while ($1)\n{\n\n}" "while-proc" nil
                        ("procedures")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/procedures/while-proc" nil nil)
                       ("sample" "$1 <$ $2;" "sample" nil
                        ("procedures")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/procedures/sample" nil nil)
                       ("proc-no-return" "proc $1 ($2: $3) = {\n\n}" "proc-no-return" nil
                        ("procedures")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/procedures/proc-no-return" nil nil)
                       ("proc-call" "$1 <@ $2;" "proc-call" nil
                        ("procedures")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/procedures/proc-call" nil nil)
                       ("proc-abstract" "proc $1($2: $3) : $4" "proc-abstract" nil
                        ("procedures")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/procedures/proc-abstract" nil nil)
                       ("proc" "proc $1($2 : $3) : $4 = {\n\n    return $5;\n}" "proc" nil
                        ("procedures")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/procedures/proc" nil nil)
                       ("new-var" "var $1 : $2;" "new-var" nil
                        ("procedures")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/procedures/new-var" nil nil)
                       ("init-var" "$1 <- witness;" "init-var" nil
                        ("procedures")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/procedures/init-var" nil nil)
                       ("increment-counter" "$1 <- $1 + 1;" "increment-counter" nil
                        ("procedures")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/procedures/increment-counter" nil nil)
                       ("if" "if ($1) {\n\n}" "if" nil
                        ("procedures")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/procedures/if-proc" nil nil)
                       ("if-else-proc" "if ($1){\n\n    } else {\n\n}" "if-else-proc" nil
                        ("procedures")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/procedures/if-else-proc" nil nil)
                       ("decrement-counter" "$1 <- $1 - 1;" "decrement-counter" nil
                        ("procedures")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/procedures/decrement-counter" nil nil)
                       ("assign" "$1 <- $2;" "assign" nil
                        ("procedures")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/procedures/assign" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'easycrypt-mode
                     '(("small-lemma" "lemma $1 : $2 by $3." "small-lemma" nil nil
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/propositions/small-lemma" nil nil)
                       ("phoare" "phoare :\n  [   $1\n      ==>\n      $2\n  ] = 1%r.\nproof.\n\nqed.\n" "phoare" nil
                        ("propositions")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/propositions/phoare" nil nil)
                       ("op-no-return" "op $1 ($2: $3) = $4." "op-no-return" nil nil
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/propositions/op-no-return" nil nil)
                       ("op-barebones" "op $1 = $2." "op-barebones" nil nil
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/propositions/op-barebones" nil nil)
                       ("op-axiomatized" "op $1 = $2 axiomatized by $3." "op-axiomatized" nil nil
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/propositions/op-axiomatixed" nil nil)
                       ("op-as" "op $1 : {$2 | $3} as $4." "op-as" nil nil
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/propositions/op-as" nil nil)
                       ("local" "local $1" "local" nil
                        ("propositions")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/propositions/local" nil nil)
                       ("lemma-phoare" "lemma $1 :\n  phoare [\n      $2\n      ==>\n      $3\n  ] = 1%r.\nproof.\n\nqed.\n" "lemma-phoare" nil
                        ("propositions")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/propositions/lemma_phoare" nil nil)
                       ("lemma-islossless" "lemma $1 : islossless\n  $2.\nproof.\n\nqed.\n" "lemma-islossless" nil
                        ("propositions")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/propositions/lemma_islossless" nil nil)
                       ("lemma-hoare" "lemma $1 :\n  hoare [\n      $2\n      ==>\n      $3\n  ].\nproof.\n\nqed.\n" "lemma-hoare" nil
                        ("propositions")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/propositions/lemma_hoare" nil nil)
                       ("lemma-equiv" "lemma $1 :\n  equiv [\n      $2 ~ $3\n      $4\n      ==>\n      $5\n  ].\nproof.\n\nqed.\n" "lemma-equiv" nil
                        ("propositions")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/propositions/lemma_equiv" nil nil)
                       ("lemma-equality" "lemma  $1 :\n  ($2) = ($3).\nproof.\n$4\nqed." "lemma-equality" nil nil
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/propositions/lemma_equality" nil nil)
                       ("lemma" "lemma $1 :\n  $2.\nproof.\n\nqed.\n" "lemma" nil
                        ("propositions")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/propositions/lemma" nil nil)
                       ("hoare" "hoare $1 : $2 :\n      $3\n      ==>\n      $4.\nproof.\n\nqed.\n" "hoare" nil
                        ("propositions")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/propositions/hoare" nil nil)
                       ("equiv" "equiv $1 : $2 ~ $3 :\n    $4\n    ==>\n    $5 .\nproof.\n\nqed.\n" "equiv" nil
                        ("propositions")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/propositions/equiv" nil nil)
                       ("declare" "declare $1\n" "declare" nil
                        ("propositions")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/propositions/declare" nil nil)
                       ("axiomatized" "axiomatized by $1." "axiomatized" nil nil
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/propositions/axiomatized" nil nil)
                       ("axiom" "axiom $1 : $2.\n" "axiom" nil
                        ("propositions")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/propositions/axiom" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'easycrypt-mode
                     '(("" "while{2} ($1)" "while2" nil
                        ("tactics")
                        nil "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/while2" nil nil)
                       ("while1" "while{1} ($1)" "while1" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/while1" nil nil)
                       ("while-true" "while true $1" "while-true" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/while-true" nil nil)
                       ("while" "while(\n\n)." "while" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/while" nil nil)
                       ("unroll" "unroll for{$1} ^while" "unroll-while" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/unroll-while" nil nil)
                       ("unroll-prog" "unroll {$1} $2" "unroll-equiv" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/unroll-equiv" nil nil)
                       ("transitivity-code" "transitivity{$1} {$2;}\n      ($2 ==> $3)\n      ($4 ==> $5)." "transitivity-code" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/transitivity-code" nil nil)
                       ("transitivity" "transitivity $1\n    ($2 ==> $3)\n    ($4 ==> $5)." "transitivity" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/transitivity" nil nil)
                       ("splitwhile2" "splitwhile{2} $1 : ($2)" "splitwhile2" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/splitwhile2" nil nil)
                       ("splitwhile1" "splitwhile{1} $1 : ($2)" "splitwhile1" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/splitwhile1" nil nil)
                       ("splitwhile" "splitwhile $1 : ($2)" "splitwhile" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/splitwhile" nil nil)
                       ("smt" "smt()." "smt" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/smt" nil nil)
                       ("sim" "sim ($1) / ($2) : ($3)" "sim" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/sim" nil nil)
                       ("seq" "seq $1 $2 : ($3)" "seq" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/seq" nil nil)
                       ("rnd2" "rnd{2}" "rnd2" nil
                        ("tactics")
                        nil "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/rnd2" nil nil)
                       ("rnd1" "rnd{1}" "rnd1" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/rnd1" nil nil)
                       ("rnd-fg" "rnd ($1) ($2)" "rnd-fg" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/rnd-fg" nil nil)
                       ("rnd-f" "rnd ($1)" "rnd-f" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/rnd-f" nil nil)
                       ("rewrite-proof-term-in" "rewrite /$1 in $2" "rewrite-proof-term-in" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/rewrite-proof-term-in" nil nil)
                       ("rewrite-lemma-forward-args" "rewrite ($1 $2)." "rewrite-lemma-forwards-args" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/rewrite-lemma-forwards-args" nil nil)
                       ("rewrite-in" "rewrite $1 in $2" "rewrite-in" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/rewrite-in" nil nil)
                       ("rewrite-lemma-backwards-args" "rewrite -($1 $2)" "rewrite-lemma-backwards-args" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/rewrite-fun-backwards" nil nil)
                       ("rewrite-expand-list" "rewrite $1 !/mkseq -iotaredE => />." "rewrite-expand-list" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/rewrite-expand-list" nil nil)
                       ("rewrite-crush" "rewrite $1 => />." "rewrite-crush" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/rewrite-crush" nil nil)
                       ("rewrite" "rewrite $1" "rewrite" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/rewrite" nil nil)
                       ("repeat-from-to" "[$1..$2]!$3" "repeat-from-to" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/repeat-from-to" nil nil)
                       ("rcondt2" "rcondt{2} $1" "rcondt2" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/rcondt2" nil nil)
                       ("rcondt1" "rcondt{1} $1" "rcondt1" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/rcondt1" nil nil)
                       ("rcondt-prog" "rcondt {$1} $2" "rcondt-equiv" nil nil nil "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/rcondt-rl" nil nil)
                       ("rcondf2" "rcondf{2} $1" "rcondf2" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/rcondf2" nil nil)
                       ("rcondf1" "rcondf{1} $1" "rcondf1" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/rcondf1" nil nil)
                       ("proc-up-to-bad2" "proc $1 ($2) ($3)" "proc-up-to-bad2" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/proc-up-to-bad2" nil nil)
                       ("proc-up-to-bad" "proc $1 ($2)" "proc-up-to-bad" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/proc-up-to-bad" nil nil)
                       ("proc-simplify" "proc => //=." "proc-simplify" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/proc-simplify" nil nil)
                       ("pose" "pose $1 := $2." "pose" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/pose" nil nil)
                       ("move-to-assumptions" "move => $1" "move-to-assumption" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/move-to-assumption" nil nil)
                       ("move-lemma-args" "move: ($1 $2)" "move-lemma-args" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/move-lemma-args" nil nil)
                       ("move-hr" "move => &hr $1" "move-hr" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/move-hr" nil nil)
                       ("move-from-assumption" "move : $1." "move-from-assumption" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/move-from-assumption" nil nil)
                       ("move-2" "move => &2 $1" "move-2" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/move-2" nil nil)
                       ("move-1m2" "move => &1 &m &2 $1" "move-1m2" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/move-1m2" nil nil)
                       ("move-1" "move => &1 $1" "move-1" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/move-1" nil nil)
                       ("lastn" "τ1; last $1 $2" "lastn" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/lastn" nil nil)
                       ("last-n-first" "$1; last $2 first" "last-n-first" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/last-n-first" nil nil)
                       ("last-first" "$1; last first" "last-first" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/last-first" nil nil)
                       ("last" "$1; last $2" "last" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/last" nil nil)
                       ("inline2" "inline{2} ($1)." "inline2" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/inline2" nil nil)
                       ("inline1" "inline{1} ($1)." "inline1" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/inline1" nil nil)
                       ("inline" "inline {$1} ($2)." "inline" nil nil nil "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/inline" nil nil)
                       ("implies-brackets" "($1 => $2)" "implies-brackets" nil
                        ("tactics")
                        nil "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/implies-brackets" nil nil)
                       ("implies" "$1 => $2" "implies" nil
                        ("tactics")
                        nil "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/implies" nil nil)
                       ("if2" "if{2}" "if2" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/if2" nil nil)
                       ("if1" "if{1}" "if1" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/if1" nil nil)
                       ("if" "if ($1) {\n\n}" "if" nil nil nil "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/if" nil nil)
                       ("have-wrtie-right-eq" "have ->: ($1) = ($2)." "have-write-right-eq" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/have-write-right-eq" nil nil)
                       ("have-write-right" "have ->: $1." "have-write-right" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/have-write-right" nil nil)
                       ("have-write-left-eq" "have <-: ($1) = ($2)." "have-write-left-eq" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/have-write-left-eq" nil nil)
                       ("have-write-left" "have <-: $1." "have-write-left" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/have-write-left" nil nil)
                       ("have-introduce" "have $1: $2." "have-introduce" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/have-introduce" nil nil)
                       ("have-define" "have := $1." "have-define" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/have-define" nil nil)
                       ("fusion2" "fusion{2} $1!$2 @ $3, $4" "fusion2" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/fusion2" nil nil)
                       ("fusion1" "fusion{1} $1!$2 @ $3, $4" "fusion1" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/fusion1" nil nil)
                       ("fusion" "fusion $1!$2 @ $3, $4" "fusion" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/fusion" nil nil)
                       ("fission2" "fission{2} $1!$2 @ $3, $4" "fission2" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/fission2" nil nil)
                       ("fission1" "fission{1} $1!$2 @ $3, $4" "fission1" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/fission1" nil nil)
                       ("fission" "fission $1!$2 @ $3, $4" "fission" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/fission" nil nil)
                       ("firstn" "$1; first $2 $3" "firstn" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/firstn" nil nil)
                       ("" "$1; first $2 last" "first-n-last" nil
                        ("tactics")
                        nil "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/first-n-last" nil nil)
                       ("first-last" "$1; first last" "first-last" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/first-last" nil nil)
                       ("exists-astreix" "exists* ($1)" "exists-astrix" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/exists-astrix" nil nil)
                       ("exists-astreix1" "exists* ($1){1}, ($2)" "exists-astreix1" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/exists-astreix1" nil nil)
                       ("exists" "exists ($1)" "exists" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/exists" nil nil)
                       ("elim-var" "elim: $1" "elim-var" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/elim-var" nil nil)
                       ("elim-proof-term" "elim /$1" "elim-proof-term" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/elim-proof-term" nil nil)
                       ("ecall-wp" "ecall ($1); wp." "ecall-wp" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/ecall-wp" nil nil)
                       ("ecall" "ecall ($1)." "ecall" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/ecall" nil nil)
                       ("dosplit" "do split.\n    + $1" "dosplit" nil nil nil "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/dosplit" nil nil)
                       ("dosplit" "do split.\n    + $1" "dosplit" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/do-split" nil nil)
                       ("do-n-times" "do $1! ($2)." "do-n-times" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/do-n-times" nil nil)
                       ("conseq2-true-true" "conseq{2} (_ : true ==> true)" "conseq2-true-true" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/conseq2-true-true" nil nil)
                       ("conseq2-true" "conseq{2} (_ : true)" "conseq2-true" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/conseq2-true" nil nil)
                       ("conseq2-invar" "conseq{2} ($1)" "conseq2-invar" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/conseq2-invar" nil nil)
                       ("conseq2" "conseq{2} (_ : $1 ==> $2)" "conseq2" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/conseq2" nil nil)
                       ("conseq1-true-true" "conseq{1} (_ : true ==> true)" "conseq1-true-true" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/conseq1-true-true" nil nil)
                       ("conseq1-true" "conseq{1} (_ : true)" "conseq1-true" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/conseq1-true" nil nil)
                       ("conseq1-invar" "conseq{1} ($1)" "conseq1-invar" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/conseq1-invar" nil nil)
                       ("conseq1" "conseq{1} (_ : $1 ==> $2)" "conseq1" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/conseq1" nil nil)
                       ("conseq-true-true" "conseq (_: true ==> true)" "conseq-true-true" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/conseq-true-true" nil nil)
                       ("conseq-true" "conseq (_: _ ==> true)" "conseq-true" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/conseq-true" nil nil)
                       ("conseq-invar" "conseq (_ : $1)" "conseq-invar" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/conseq-invar" nil nil)
                       ("conseq" "conseq (_ : $1 ==> $2)" "conseq" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/conseq" nil nil)
                       ("clear" "clear $1." "clear" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/clear" nil nil)
                       ("cfold_from_to2" "cfold{2} $1 ! $2" "cfold_from_to2" nil nil nil "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/cfold_from_to2" nil nil)
                       ("cfold_from_to1" "cfold{1} $1 ! $2" "cfold_from_to1" nil nil nil "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/cfold_from_to1" nil nil)
                       ("cfold_from_to" "cfold $1 ! $2" "cfold_from_to" nil nil nil "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/cfold_from_to" nil nil)
                       ("cfold2" "cfold{2} $1" "cfold2" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/cfold2" nil nil)
                       ("cfold1" "cfold{1} $1" "cfold1" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/cfold1" nil nil)
                       ("cfold-from-to2" "cfold{2} $1 ! $2" "cfold-from-to2" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/cfold-from-to2" nil nil)
                       ("cfold-from-to1" "cfold{1} $1 ! $2" "cfold-from-to1" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/cfold-from-to1" nil nil)
                       ("cfold-from-to" "cfold $1 ! $2" "cfold-from-to" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/cfold-from-to" nil nil)
                       ("cfold" "cfold $1" "cfold" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/cfold" nil nil)
                       ("case-move" "case: ($1{1}) => $2" "case-move" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/case-move" nil nil)
                       ("case" "case: ($1{1})" "case" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/case" nil nil)
                       ("call2-true-true" "call{2} (_ : true ==> true)" "call2-true-true" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/call2-true-true" nil nil)
                       ("call2-true" "call{2} (_ : true)" "call2-true" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/call2-true" nil nil)
                       ("call2-invar" "call{2} ($1)" "call2-invar" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/call2-invar" nil nil)
                       ("call2" "call{2} (_ : $1 ==> $2)" "call2" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/call2" nil nil)
                       ("call1-true-true" "call{1} (_ : true ==> true)" "call1-true-true" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/call1-true-true" nil nil)
                       ("call1-true" "call{1} (_ : true)" "call1-true" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/call1-true" nil nil)
                       ("call1-invar" "call{1} ($1)" "call1-invar" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/call1-invar" nil nil)
                       ("call1" "call{1} (_ : $1 ==> $2)" "call1" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/call1" nil nil)
                       ("call-true-true" "call(_: true ==> true)." "call-true-true" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/call-true-true" nil nil)
                       ("call-true" "call (_: true)" "call-true" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/call-true" nil nil)
                       ("call-invar3" "call (_: $1, $2, $3)" "call-invar3" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/call-invar3" nil nil)
                       ("call-invar2" "call (_: $1, $2)" "call-invar2" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/call-invar2" nil nil)
                       ("call-invar" "call (_: $1)" "call-invar" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/call-invar" nil nil)
                       ("call" "call (_ : $1 ==> $2)" "call" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/call" nil nil)
                       ("bypr" "bypr ($1){1} ($2){2}" "bypr" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/bypr" nil nil)
                       ("nyphoare" "byphoare (_ : $1 ==> $2)" "byphoare" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/byphoare" nil nil)
                       ("byequiv" "byequiv (_ : $1 ==> $2)" "byequiv" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/byequiv" nil nil)
                       ("bad-smt" "smt." "bad-smt" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/bad-smt" nil nil)
                       ("auto" "auto => />.\n" "auto" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/auto.yasnippet" nil nil)
                       ("auto" "auto => />.\n" "auto" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/auto" nil nil)
                       ("async-while" "async while\n     [ (fun r => $1), ($2) ]\n     [ (fun r => $3), ($2) ]\n        ($4) ($5)\n     :\n     ($6)." "async-while" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/async-while" nil nil)
                       ("apply-proof-term" "apply /$1" "apply-proof-term" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/apply-proof-term" nil nil)
                       ("apply-in" "apply $1 in $2" "apply-in" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/apply-in" nil nil)
                       ("apply-args" "apply ($1 $2)." "apply-args" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/apply-args" nil nil)
                       ("aliaswith2" "alias{2} $1 with $2" "aliaswith2" nil nil nil "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/aliaswith2" nil nil)
                       ("aliaswith1" "alias{1} $1 with $2" "aliaswith1" nil nil nil "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/aliaswith1" nil nil)
                       ("aliaswith" "alias $1 with $2" "aliaswith" nil nil nil "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/aliaswith" nil nil)
                       ("alias-with2" "alias{2} $1 with $2" "alias-with2" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/alias2-with" nil nil)
                       ("alias2" "alias{2} $1 $2 = $3" "alias2" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/alias2" nil nil)
                       ("alias-with1" "alias{1} $1 with $2" "alias-with1" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/alias1-with" nil nil)
                       ("alias1" "alias{1} $1 $2 = $3" "alias1" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/alias1" nil nil)
                       ("alias-with" "alias $1 with $2" "alias-with" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/alias-with" nil nil)
                       ("alias" "alias $1 $2 = $3" "alias" nil
                        ("tactics")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/tactics/alias" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'easycrypt-mode
                     '(("with" "with $1 => $2" "with" nil
                        ("theories")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/theories/with" nil nil)
                       ("type" "type $1 = $2." "type" nil
                        ("theories")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/theories/type" nil nil)
                       ("theory" "theory $1.\n\n\nend $1." "theory" nil
                        ("theories")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/theories/theory" nil nil)
                       ("section" "section.\n\nend section." "section" nil
                        ("theories")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/theories/section" nil nil)
                       ("rename-as" "rename \"$1\" as \"$2\"" "rename-as" nil
                        ("theories")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/theories/rename-as" nil nil)
                       ("clone-no-with" "clone $1 as $2." "clone-no-with" nil nil nil "/Users/yojan/.emacs.d/snippets/easycrypt-mode/theories/clone-no-with" nil nil)
                       ("clone-include" "clone include $1" "clone-include" nil
                        ("theories")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/theories/clone-include" nil nil)
                       ("clone-import" "clone import $1 as $2 with" "clone-import" nil
                        ("theories")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/theories/clone-import" nil nil)
                       ("clone-as-with" "clone $1 as $2 with" "clone-as-with" nil
                        ("theories")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/theories/clone-as-with" nil nil)
                       ("clone-as" "clone $1 as $2." "clone-as" nil
                        ("theories")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/theories/clone-as" nil nil)
                       ("abstract-theory" "abstract theory $1.\n\n\nend $1." "abstract-theory" nil
                        ("theories")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/easycrypt-mode/theories/abstract-theory" nil nil)))


;;; Do not edit! File generated at Thu Nov 20 13:20:18 2025
