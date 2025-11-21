;;; "Compiled" snippets and support files for `python-mode'  -*- lexical-binding:t -*-
;;; contents of the .yas-setup.el support file:
;;;
;;; -*- lexical-binding: t -*-

; Copyright (C) miscellaneous contributors, see git history
; Copyright (C) 2024 Daniel Hornung <d.hornung@indiscale.com>
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as
; published by the Free Software Foundation, either version 3 of the
; License, or (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program. If not, see <https://www.gnu.org/licenses/>.

(require 'yasnippet)
(defvar yas-text)

(defvar yas-python-regex-identifier "[[:alnum:]_]+" "Simplified Python identifier.")
(defvar yas-python-regex-quoted-or-identifier (concat
                                               "\\("
                                               yas-python-regex-identifier
                                               "\\)"
                                               "\\|" "\".*\""
                                               "\\|" "'.*'"
                                               )
  "Simplified Python identifier or quoted string.
Does not work well with multiple or escaped quotes")

(defvar python-split-arg-regex
  (concat
   "\\(" yas-python-regex-identifier "\\)"     ; name
   "\\(:[[:blank:]]*\\([][:alpha:]_[]*\\)\\)?" ; type
   "\\([[:blank:]]*=[[:blank:]]*\\("
   yas-python-regex-quoted-or-identifier       ; default
   "\\)\\)?"
   )
"Regular expression matching an argument of a python function.
Groups:
- 1: the argument name
- 3: the type
- 5: the default value")

(defvar python-split-arg-separator
"[[:space:]]*,[[:space:]]*"
"Regular expression matching the separator in a list of argument.")

(defun python-split-args (arg-string)
  "Split python argument string ARG-STRING.

The result is a list ((name, type, default), ...) of argument names, types and
default values.  An argument named `self` is omitted."
  (remove
   nil
   (mapcar (lambda (x)           ; organize output
             (when (and
                    (not (equal "self" x))
                    (string-match python-split-arg-regex x)
                    )
               (list
                (match-string-no-properties 1 x) ; name
                (match-string-no-properties 3 x) ; type
                (match-string-no-properties 5 x) ; default
                )))
           (split-string arg-string python-split-arg-separator t))))

(defun python-args-to-docstring ()
  "Return docstring format for the python arguments in yas-text."
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (python-split-args yas-text))
         (max-len (if args (apply 'max (mapcar (lambda (x) (length (nth 0 x))) args)) 0))
         (formatted-args (mapconcat
                          (lambda (x)
                            (concat (nth 0 x) (make-string (- max-len (length (nth 0 x))) ? ) " -- "
                                    (if (nth 1 x) (concat (nth 1 x) ": "))
                                    (if (nth 2 x) (concat "\(default " (nth 2 x) "\)"))
                                    ))
                          args
                          indent)))
    (unless (string= formatted-args "")
      (mapconcat 'identity (list "Keyword Arguments:" formatted-args) indent))))

(defun python-args-to-docstring-numpy ()
  "return docstring format for the python arguments in yas-text"
  (let* ((args (python-split-args yas-text))
         (format-arg (lambda(arg)
                       (concat (nth 0 arg) " : "            ; name
                               (if (nth 1 arg) (nth 1 arg)) ; type  TODO handle Optional[Foo] correctly
                               (if (nth 2 arg) (concat (when (nth 1 arg) ", ")
                                                       "default=" (nth 2 arg))) ; default
                               "\n")))
         (formatted-params (mapconcat format-arg args "\n"))
         (formatted-ret (mapconcat format-arg (list (list "out")) "\n")))
    (unless (string= formatted-params "")
      (mapconcat 'identity
                 (list "\nParameters\n----------" formatted-params
                       "\nReturns\n-------" formatted-ret)
                 "\n"))))


;; Tests
;; (ert-deftest test-split ()
;;   "For starters, only test a single string for expected output."
;;   (should (equal
;;            (python-split-args "_foo='this', bar: int = 2, baz: Optional[My_Type], foobar")
;;            (list '("_foo" nil "'this'")
;;                  '("bar" "int" "2")
;;                  '("baz" "Optional[My_Type]" nil)
;;                  '("foobar" nil nil)))
;;   ))

;; (ert-deftest test-argument-self ()
;;   "If an argument is called `self`, it must be omitted"
;;   (should (equal
;;            (python-split-args "self, _foo=\"this\"")
;;            (list '("_foo" nil "\"this\"")
;;                  ))
;;   ))

;; For manual testing and development:

;; (setq yas-text "foo=3, bar: int = 2, baz: Optional[MyType], foobar")
;; (split-string yas-text python-split-arg-separator t)
;;
;; (save-match-data
;;   (setq my-string "_foo: my_bar = 'this'")
;;   (string-match python-split-arg-regex my-string)
;;   (match-string 5 my-string)
;;   )
;;
;; (python-split-args yas-text)
;; (python-args-to-docstring)
;; (python-args-to-docstring-numpy)
;;; Snippet definitions:
;;;
(yas-define-snippets 'python-mode
                     '(("wo" "with open(${1:\"filename\"}${2:, encoding=\"${3:utf-8}\"}${4:, mode=\"${5:w}\"}) as ${6:myfile}:\n    $0" "with-open" nil
                        ("control structure")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/with-open" nil nil)
                       ("with" "with ${1:expr}${2: as ${3:alias}}:\n    $0\n" "with" nil
                        ("control structure")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/with" nil nil)
                       ("wh" "while ${1:True}:\n    $0\n" "while" nil
                        ("control structure")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/while" nil nil)
                       ("uv" "#!/usr/bin/uv run\n# /// script\n# dependencies = [\n# $1\n# ]\n# ///\n$0" "uv-script" nil nil nil "/Users/yojan/.emacs.d/snippets/python-mode/uv-script" nil nil)
                       ("utf8" "# -*- coding: utf-8 -*-\n" "utf-8 encoding" nil nil nil "/Users/yojan/.emacs.d/snippets/python-mode/utf8" nil nil)
                       ("tryelse" "try:\n    $0\nexcept $1:\n    $2\nelse:\n    $3\n" "tryelse" nil nil
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/python-mode/tryelse" nil nil)
                       ("try" "try:\n    $0\nexcept ${1:Exception}:\n    $2\n" "try" nil nil
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/python-mode/try" nil nil)
                       ("tf" "import unittest\n${1:from ${2:test_file} import *}\n\n$0\n\nif __name__ == '__main__':\n    unittest.main()\n" "test_file" nil
                        ("testing")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/test_file" nil nil)
                       ("tcs" "class Test${1:toTest}(${2:unittest.TestCase}):\n    $0\n" "test_class" nil
                        ("testing")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/test_class" nil nil)
                       ("sm" "@staticmethod\ndef ${1:func}($0):\n" "static" nil nil nil "/Users/yojan/.emacs.d/snippets/python-mode/static" nil nil)
                       ("size" "sys.getsizeof($0)\n" "size" nil nil nil "/Users/yojan/.emacs.d/snippets/python-mode/size" nil nil)
                       ("setup" "from setuptools import setup\n\npackage = '${1:name}'\nversion = '${2:0.1}'\n\nsetup(name=package,\n      version=version,\n      description=\"${3:description}\",\n      url='${4:url}'$0)\n" "setup" nil
                        ("distribute")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/setup" nil nil)
                       ("setdef" "${1:var}.setdefault(${2:key}, []).append(${3:value})\n" "setdef" nil nil nil "/Users/yojan/.emacs.d/snippets/python-mode/setdef" nil nil)
                       ("sn" "self.$1 = $1\n" "selfassign" nil
                        ("object oriented")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/selfassign" nil nil)
                       ("s" "self\n" "self_without_dot" nil
                        ("object oriented")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/self_without_dot" nil nil)
                       ("." "self.$0\n" "self" nil
                        ("object oriented")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/self" nil nil)
                       ("script" "#!/usr/bin/env python\n\ndef main():\n    pass\n\nif __name__ == '__main__':\n    main()\n" "script" nil nil nil "/Users/yojan/.emacs.d/snippets/python-mode/script" nil nil)
                       ("scls" "class ${1:class}(${2:super-class}):\n    $0\n" "subclass" nil
                        ("object oriented")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/scls" nil nil)
                       ("r" "return $0\n" "return" nil nil nil "/Users/yojan/.emacs.d/snippets/python-mode/return" nil nil)
                       ("reg" "${1:regexp} = re.compile(r\"${2:expr}\")\n$0\n" "reg" nil
                        ("general")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/reg" nil nil)
                       ("pudb" "import pudb; pudb.set_trace()\n" "pudb trace" nil
                        ("debug")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/pudb" nil nil)
                       ("prop" "def ${1:foo}():\n    doc = \"\"\"${2:Doc string}\"\"\"\n    def fget(self):\n        return self._$1\n\n    def fset(self, value):\n        self._$1 = value\n\n    def fdel(self):\n        del self._$1\n    return locals()\n$1 = property(**$1())\n\n$0\n" "prop" nil nil
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/python-mode/prop" nil nil)
                       ("p" "print($0)\n" "print" nil nil nil "/Users/yojan/.emacs.d/snippets/python-mode/print" nil nil)
                       ("plt" "import matplotlib.pyplot as plt\n$0\n" "Import pyplot" nil
                        ("general")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/pl" nil nil)
                       ("pdb" "breakpoint()\n" "pdb trace" nil
                        ("debug")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/pdb" nil nil)
                       ("ps" "pass\n" "pass" nil nil nil "/Users/yojan/.emacs.d/snippets/python-mode/pass" nil nil)
                       ("pars" "parser = argparse.ArgumentParser(description='$1')\n$0\n" "parser" nil
                        ("argparser")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/parser" nil nil)
                       ("pargs" "def parse_arguments():\n    parser = argparse.ArgumentParser(description='$1')\n    $0\n    return parser.parse_args()\n" "parse_args" nil
                        ("argparser")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/parse_args" nil nil)
                       ("np" "import numpy as np\n$0\n" "np" nil
                        ("general")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/np" nil nil)
                       ("not_impl" "raise NotImplementedError\n" "not_impl" nil nil nil "/Users/yojan/.emacs.d/snippets/python-mode/not_impl" nil nil)
                       ("mdn" "def ${1:name}(self$2):\n    \\\"\\\"\\\"$3\n    ${2:$(python-args-to-docstring-numpy)}\n    \\\"\\\"\\\"\n    $0\n" "method_docstring_numpy" nil
                        ("object oriented")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/method_docstring_numpy" nil nil)
                       ("md" "def ${1:name}(self$2):\n    \\\"\\\"\\\"$3\n    ${2:$(python-args-to-docstring)}\n    \\\"\\\"\\\"\n    $0\n" "method_docstring" nil
                        ("object oriented")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/method_docstring" nil nil)
                       ("m" "def ${1:method}(self${2:, $3}):\n    $0\n" "method" nil
                        ("object oriented")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/method" nil nil)
                       ("main" "def main():\n    $0\n" "main" nil nil nil "/Users/yojan/.emacs.d/snippets/python-mode/main" nil nil)
                       ("log" "logger = logging.getLogger(\"${1:name}\")\nlogger.setLevel(logging.${2:level})\n" "logging" nil nil nil "/Users/yojan/.emacs.d/snippets/python-mode/logging" nil nil)
                       ("ln" "logger = logging.getLogger(${1:__name__})\n" "logger_name" nil nil nil "/Users/yojan/.emacs.d/snippets/python-mode/logger_name" nil nil)
                       ("li" "[${1:el} for $1 in ${2:list}]\n$0\n" "list" nil
                        ("definitions")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/list" nil nil)
                       ("lam" "lambda ${1:x}: $0\n" "lambda" nil nil nil "/Users/yojan/.emacs.d/snippets/python-mode/lambda" nil nil)
                       ("ipdb" "import ipdb; ipdb.set_trace()\n" "ipdb trace" nil
                        ("debug")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/ipdb" nil nil)
                       ("int" "import code; code.interact(local=locals())\n" "interact" nil nil nil "/Users/yojan/.emacs.d/snippets/python-mode/interact" nil nil)
                       ("idn" "def __init__(self$1):\n    \\\"\\\"\\\"$2\n    ${1:$(python-args-to-docstring-numpy)}\n    \\\"\\\"\\\"\n    $0\n" "init_docstring_numpy" nil
                        ("definitions")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/init_docstring_numpy" nil nil)
                       ("id" "def __init__(self$1):\n    \\\"\\\"\\\"$2\n    ${1:$(python-args-to-docstring)}\n    \\\"\\\"\\\"\n    $0\n" "init_docstring" nil
                        ("definitions")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/init_docstring" nil nil)
                       ("imp" "import ${1:lib}${2: as ${3:alias}}\n$0\n" "import" nil
                        ("general")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/import" nil nil)
                       ("ifm" "if __name__ == '__main__':\n    ${1:main()}\n" "ifmain" nil nil nil "/Users/yojan/.emacs.d/snippets/python-mode/ifmain" nil nil)
                       ("ife" "if $1:\n    $2\nelse:\n    $0\n" "ife" nil
                        ("control structure")
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/python-mode/ife" nil nil)
                       ("if" "if ${1:cond}:\n    $0\n" "if" nil
                        ("control structure")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/if" nil nil)
                       ("ic" "from icecream import ic\nic($1)\n" "ic" nil nil nil "/Users/yojan/.emacs.d/snippets/python-mode/ic.py" nil nil)
                       ("doxy_func" "\"\"\"\n@brief      ${1:function description}\n\n@details    ${2:detailed description}\n\n@param      ${3:param}\n\n@return     ${4:return type}\n\"\"\"\n" "Function Doxygen Doc" nil
                        ("doxygen")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/function_doxygen_doc" nil nil)
                       ("fdn" "def ${1:name}($2):\n    \\\"\\\"\\\"$3\n\n${2:$(python-args-to-docstring-numpy)}\n    \\\"\\\"\\\"\n    $0\n" "function_docstring_numpy" nil
                        ("definitions")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/function_docstring_numpy" nil nil)
                       ("fd" "def ${1:name}($2):\n \\\"\\\"\\\"$3\n ${2:$(python-args-to-docstring)}\n \\\"\\\"\\\"\n $0\n" "function_docstring" nil
                        ("definitions")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/function_docstring" nil nil)
                       ("f" "def ${1:fun}(${2:args}):\n    $0\n" "function" nil
                        ("definitions")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/function" nil nil)
                       ("from" "from ${1:module} import ${2:symbol}\n$0\n" "from MOD import SYM" nil
                        ("Header")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/from" nil nil)
                       ("for" "for ${var} in ${collection}:\n    $0\n" "for ... in ... : ..." nil
                        ("control structure")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/for" nil nil)
                       ("env" "#!/usr/bin/env python\n" "#!/usr/bin/env python" nil
                        ("Header")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/env" nil nil)
                       ("en" "class ${1:class}(Enum):\n    $0\n" "enum" nil
                        ("object oriented")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/enum" nil nil)
                       ("embed" "from IPython import embed; embed()\n" "embed" nil nil nil "/Users/yojan/.emacs.d/snippets/python-mode/embed" nil nil)
                       ("doctest" ">>> ${1:function calls}\n${2:desired output}\n$0\n" "doctest" nil
                        ("testing")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/doctest" nil nil)
                       ("d" "\"\"\"$0\n\"\"\"\n" "doc" nil nil nil "/Users/yojan/.emacs.d/snippets/python-mode/doc" nil nil)
                       ("dtcs" "class ${1:Model}Test(TestCase):\n    $0\n" "django_test_class" nil
                        ("testing")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/django_test_class" nil nil)
                       ("dt" "def test_${1:long_name}(self):\n    $0\n" "deftest" nil
                        ("testing")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/deftest" nil nil)
                       ("def" "def ${1:methodname}(self, ${2:arg}):\n    ${3:pass}\n" "def method(self, ...):" nil
                        ("definitions")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/def" nil nil)
                       ("dec" "def ${1:decorator}(func):\n    $2\n    def _$1(*args, **kwargs):\n        $3\n        ret = func(*args, **kwargs)\n        $4\n        return ret\n\n    return _$1\n" "dec" nil
                        ("definitions")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/dec" nil nil)
                       ("dc" "@dataclass\nclass ${1:class}:\n    $0\n" "dataclass" nil
                        ("object oriented")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/dataclass" nil nil)
                       ("cls" "class ${1:class}:\n    $0\n" "class" nil
                        ("object oriented")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/cls" nil nil)
                       ("cm" "@classmethod\ndef ${1:meth}(cls, $2):\n    $0\n" "classmethod" nil
                        ("object oriented")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/classmethod" nil nil)
                       ("doxy_class" "\"\"\"\n@brief      ${1:class description}\n\n@details    ${2:detailed description}\n\"\"\"\n" "Class Doxygen Doc" nil
                        ("doxygen")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/class_doxygen_doc" nil nil)
                       ("cdb" "from celery.contrib import rdb; rdb.set_trace()\n" "celery pdb" nil
                        ("debug")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/celery_pdb" nil nil)
                       ("#!" "#!/usr/bin/env python\n" "#!" nil nil nil "/Users/yojan/.emacs.d/snippets/python-mode/bang" nil nil)
                       ("at" "self.assertTrue($0)\n" "assertTrue" nil
                        ("testing")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/assertTrue" nil nil)
                       ("arw" "with self.assertRaises(${1:Exception}):\n    $0\n" "assertRaisesWith" nil nil nil "/Users/yojan/.emacs.d/snippets/python-mode/assertRaises.with" nil nil)
                       ("ar" "self.assertRaises(${1:Exception}, ${2:fun})\n" "assertRaises" nil
                        ("testing")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/assertRaises" nil nil)
                       ("an" "self.assertNotIn(${1:member}, ${2:container})\n" "assetNotIn" nil
                        ("testing")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/assertNotIn" nil nil)
                       ("ane" "self.assertNotEqual($1, $2)\n" "assertNotEqual" nil
                        ("testing")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/assertNotEqual" nil nil)
                       ("ai" "self.assertIn(${1:member}, ${2:container})\n" "assertIn" nil
                        ("testing")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/assertIn" nil nil)
                       ("af" "self.assertFalse($0)\n" "assertFalse" nil
                        ("testing")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/assertFalse" nil nil)
                       ("ae" "self.assertEqual($1, $2)\n" "assertEqual" nil
                        ("testing")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/assertEqual" nil nil)
                       ("ass" "assert $0\n" "assert" nil
                        ("testing")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/assert" nil nil)
                       ("asr" "with self.assertRaises(${1:Exception}):\n    $0\n" "Assert Raises" nil
                        ("testing")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/asr" nil nil)
                       ("asne" "self.assertNotEqual(${1:expected}, ${2:actual})\n" "Assert Not Equal" nil
                        ("testing")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/asne" nil nil)
                       ("ase" "self.assertEqual(${1:expected}, ${2:actual})\n" "Assert Equal" nil
                        ("testing")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/ase" nil nil)
                       ("posarg" "parser.add_argument('${1:varname}', $0)\n" "arg_positional" nil
                        ("argparser")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/arg_positional" nil nil)
                       ("arg" "parser.add_argument('-$1', '--$2',\n                    $0)\n" "arg" nil
                        ("argparser")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/arg" nil nil)
                       ("all" "__all__ = [\n    $0\n]\n" "all" nil nil
                        ((yas-indent-line 'fixed))
                        "/Users/yojan/.emacs.d/snippets/python-mode/all" nil nil)
                       ("_xor" "def __xor__(self, other):\n    return $0\n" "__xor__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__xor__" nil nil)
                       ("_trunc" "def __trunc__(self):\n    return $0\n" "__trunc__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__trunc__" nil nil)
                       ("_truediv" "def __truediv__(self, other):\n    return $0\n" "__truediv__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__truediv__" nil nil)
                       ("_subclasscheck" "def __subclasscheck__(self, instance):\n    return $0\n" "__subclasscheck__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__subclasscheck__" nil nil)
                       ("_sub" "def __sub__(self, other):\n    return $0\n" "__sub__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__sub__" nil nil)
                       ("_str" "def __str__(self):\n    return $0\n" "__str__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__str__" nil nil)
                       ("_slots" "__slots__ = ($1)\n$0\n" "__slots__" nil
                        ("Class attributes")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__slots__" nil nil)
                       ("_setitem" "def __setitem__(self, key, value):\n    $0\n" "__setitem__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__setitem__" nil nil)
                       ("_setattr" "def __setattr__(self, name, value):\n    $0\n" "__setattr__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__setattr__" nil nil)
                       ("_set_name" "def __set_name__(self, owner, name):\n    $0\n" "__set_name__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__set_name__" nil nil)
                       ("_set" "def __set__(self, instance, value):\n    $0\n" "__set__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__set__" nil nil)
                       ("_rxor" "def __rxor__(self, other):\n    return $0\n" "__rxor__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__rxor__" nil nil)
                       ("_rtruediv" "def __rtruediv__(self, other):\n    return $0\n" "__rtruediv__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__rtruediv__" nil nil)
                       ("_rsub" "def __rsub__(self, other):\n    return $0\n" "__rsub__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__rsub__" nil nil)
                       ("_rshift" "def __rshift__(self, other):\n    return $0\n" "__rshift__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__rshift__" nil nil)
                       ("_rrshift" "def __rrshift__(self, other):\n    return $0\n" "__rrshift__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__rrshift__" nil nil)
                       ("_rpow" "def __rpow__(self, other):\n    return $0\n" "__rpow__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__rpow__" nil nil)
                       ("_round" "def __round__(self, ndigits=None):\n    return $0\n" "__round__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__round__" nil nil)
                       ("_ror" "def __ror__(self, other):\n    return $0\n" "__ror__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__ror__" nil nil)
                       ("_rmul" "def __rmul__(self, other):\n    return $0\n" "__rmul__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__rmul__" nil nil)
                       ("_rmod" "def __rmod__(self, other):\n    return $0\n" "__rmod__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__rmod__" nil nil)
                       ("_rmatmul" "def __rmatmul__(self, other):\n    return $0\n" "__rmatmul__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__rmatmul__" nil nil)
                       ("_rlshift" "def __rlshift__(self, other):\n    return $0\n" "__rlshift__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__rlshift__" nil nil)
                       ("_rfloordiv" "def __rfloordiv__(self, other):\n    return $0\n" "__rfloordiv__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__rfloordiv__" nil nil)
                       ("_reversed" "def __reversed__(self):\n    return $0\n" "__reversed__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__reversed__" nil nil)
                       ("_repr" "def __repr__(self):\n    return $0\n" "__repr__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__repr__" nil nil)
                       ("_rdivmod" "def __rdivmod__(self, other):\n    return $0\n" "__rdivmod__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__rdivmod__" nil nil)
                       ("_rand" "def __rand__(self, other):\n    return $0\n" "__rand__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__rand__" nil nil)
                       ("_radd" "def __radd__(self, other):\n    return $0\n" "__radd__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__radd__" nil nil)
                       ("_prepare" "def __prepare__(name, bases, **kwds):\n    return ${0:\\{\\}}\n" "__prepare__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__prepare__" nil nil)
                       ("_pow" "def __pow__(self, other, modulo=None):\n    return $0\n" "__pow__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__pow__" nil nil)
                       ("_pos" "def __pos__(self):\n    return $0\n" "__pos__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__pos__" nil nil)
                       ("_or" "def __or__(self, other):\n    return $0\n" "__or__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__or__" nil nil)
                       ("_next" "def __next__(self):\n    $0\n" "__next__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__next__" nil nil)
                       ("_new" "def __new__(mcs, name, bases, dct):\n    $0\n    return type.__new__(mcs, name, bases, dct)\n" "__new__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__new__" nil nil)
                       ("_neg" "def __neg__(self):\n    return $0\n" "__neg__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__neg__" nil nil)
                       ("_ne" "def __ne__(self, other):\n    return $0\n" "__ne__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__ne__" nil nil)
                       ("_mul" "def __mul__(self, other):\n    return $0\n" "__mul__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__mul__" nil nil)
                       ("_mod" "def __mod__(self, other):\n    return $0\n" "__mod__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__mod__" nil nil)
                       ("_missing" "def __missing__(self, key):\n    return $0\n" "__missing__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__missing__" nil nil)
                       ("_matmul" "def __matmul__(self, other):\n    return $0\n" "__matmul__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__matmul__" nil nil)
                       ("_lt" "def __lt__(self, other):\n    return $0\n" "__lt__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__lt__" nil nil)
                       ("_lshift" "def __lshift__(self, other):\n    return $0\n" "__lshift__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__lshift__" nil nil)
                       ("_length_hint" "def __length_hint__(self):\n    return $0\n" "__length_hint__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__length_hint__" nil nil)
                       ("_len" "def __len__(self):\n    return $0\n" "__len__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__len__" nil nil)
                       ("_le" "def __le__(self, other):\n    return $0\n" "__le__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__le__" nil nil)
                       ("_ixor" "def __ixor__(self, other):\n    return $0\n" "__ixor__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__ixor__" nil nil)
                       ("_itruediv" "def __itruediv__(self, other):\n    return $0\n" "__itruediv__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__itruediv__" nil nil)
                       ("_iter" "def __iter__(self):\n    $0\n" "__iter__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__iter__" nil nil)
                       ("_isub" "def __isub__(self, other):\n    return $0\n" "__isub__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__isub__" nil nil)
                       ("_irshift" "def __irshift__(self, other):\n    return $0\n" "__irshift__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__irshift__" nil nil)
                       ("_ipow" "def __ipow__(self, other):\n    return $0\n" "__ipow__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__ipow__" nil nil)
                       ("_ior" "def __ior__(self, other):\n    return $0\n" "__ior__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__ior__" nil nil)
                       ("_invert" "def __invert__(self):\n    return $0\n" "__invert__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__invert__" nil nil)
                       ("_int" "def __int__(self):\n    $0\n" "__int__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__int__" nil nil)
                       ("_instancecheck" "def __instancecheck__(self, instance):\n    return $0\n" "__instancecheck__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__instancecheck__" nil nil)
                       ("_init_subclass" "def __init_subclass__(cls, /${1:, param}, **kwargs):\n    super().__init_subclass__(**kwargs)\n    $0\n" "__init_subclass__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__init_subclass__" nil nil)
                       ("_init" "def __init__(self${1:, args}):\n    ${2:\"${3:docstring}\"\n    }$0\n" "__init__" nil
                        ("definitions")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__init__" nil nil)
                       ("_index" "def __index__(self):\n    return $0\n" "__index__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__index__" nil nil)
                       ("_imul" "def __imul__(self, other):\n    return $0\n" "__imul__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__imul__" nil nil)
                       ("_imod" "def __imod__(self, other):\n    return $0\n" "__imod__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__imod__" nil nil)
                       ("_imatmul" "def __imatmul__(self, other):\n    return $0\n" "__imatmul__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__imatmul__" nil nil)
                       ("_ilshift" "def __ilshift__(self, other):\n    return $0\n" "__ilshift__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__ilshift__" nil nil)
                       ("_ifloordiv" "def __ifloordiv__(self, other):\n    return $0\n" "__ifloordiv__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__ifloordiv__" nil nil)
                       ("_idiv" "def __idiv__(self, other):\n    return $0\n" "__idiv__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__idiv__" nil nil)
                       ("_iand" "def __iand__(self, other):\n    return $0\n" "__iand__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__iand__" nil nil)
                       ("_iadd" "def __iadd__(self, other):\n    return $0\n" "__iadd__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__iadd__" nil nil)
                       ("_hash" "def __hash__(self):\n    return $0\n" "__hash__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__hash__" nil nil)
                       ("_gt" "def __gt__(self, other):\n    return $0\n" "__gt__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__gt__" nil nil)
                       ("_getitem" "def __getitem__(self, key):\n    return $0\n" "__getitem__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__getitem__" nil nil)
                       ("_getattribute" "def __getattribute__(self, name):\n    return $0\n" "__getattribute__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__getattribute__" nil nil)
                       ("_getattr" "def __getattr__(self, name):\n    return $0\n" "__getattr__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__getattr__" nil nil)
                       ("_get" "def __get__(self, instance, owner=None):\n    return $0\n" "__get__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__get__" nil nil)
                       ("_ge" "def __ge__(self, other):\n    return $0\n" "__ge__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__ge__" nil nil)
                       ("_format" "def __format__(self, format_spec):\n    return $0\n" "__format__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__format__" nil nil)
                       ("_floordiv" "def __floordiv__(self, other):\n    return $0\n" "__floordiv__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__floordiv__" nil nil)
                       ("_floor" "def __floor__(self):\n    return $0\n" "__floor__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__floor__" nil nil)
                       ("_float" "def __float__(self):\n    return $0\n" "__float__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__float__" nil nil)
                       ("_exit" "def __exit__(self, exc_type, exc_value, traceback):\n    $0\n" "__exit__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__exit__" nil nil)
                       ("_eq" "def __eq__(self, other):\n    return $0\n" "__eq__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__eq__" nil nil)
                       ("_enter" "def __enter__(self):\n    $0\n\n    return self\n" "__enter__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__enter__" nil nil)
                       ("_divmod" "def __divmod__(self, other):\n    return $0\n" "__divmod__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__divmod__" nil nil)
                       ("_div" "def __div__(self, other):\n    return $0\n" "__div__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__div__" nil nil)
                       ("_dir" "def __dir__(self):\n    return $0\n" "__dir__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__dir__" nil nil)
                       ("_delitem" "def __delitem__(self, key):\n    $0\n" "__delitem__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__delitem__" nil nil)
                       ("_delete" "def __delete__(self, instance):\n    $0\n" "__delete__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__delete__" nil nil)
                       ("_delattr" "def __delattr__(self, name):\n    $0\n" "__delattr__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__delattr__" nil nil)
                       ("_del" "def __del__(self):\n    $0\n" "__del__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__del__" nil nil)
                       ("_contains" "def __contains__(self, item):\n    return $0\n" "__contains__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__contains__" nil nil)
                       ("_complex" "def __complex__(self):\n    return $0\n" "__complex__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__complex__" nil nil)
                       ("_cmp" "def __cmp__(self, other):\n    return $0\n" "__cmp__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__cmp__" nil nil)
                       ("_class_getitem" "def __class_getitem__(cls, key):\n    return $0\n" "__class_getitem__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__class_getitem__" nil nil)
                       ("_ceil" "def __ceil__(self):\n    return $0\n" "__ceil__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__ceil__" nil nil)
                       ("_call" "def __call__(self, ${1:*args}):\n    return $0\n" "__call__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__call__" nil nil)
                       ("_bytes" "def __bytes__(self):\n    return $0\n" "__bytes__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__bytes__" nil nil)
                       ("_bool" "def __bool__(self):\n    return $0\n" "__bool__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__bool__" nil nil)
                       ("_await" "def __await__(self):\n    $0\n" "__await__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__await__" nil nil)
                       ("_anext" "async def __anext__(self):\n    return $0\n" "__anext__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__anext__" nil nil)
                       ("_and" "def __and__(self, other):\n    return $0\n" "__and__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__and__" nil nil)
                       ("_aiter" "def __aiter__(self):\n    return $0\n" "__aiter__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__aiter__" nil nil)
                       ("_aexit" "async def __aexit__(self, exc_type, exc_value, traceback):\n    $0\n" "__aexit__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__aexit__" nil nil)
                       ("_aenter" "async def __aenter__(self):\n    $0\n    return self\n" "__aenter__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__aenter__" nil nil)
                       ("_add" "def __add__(self, other):\n    return $0\n" "__add__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__add__" nil nil)
                       ("_abs" "def __abs__(self):\n    return $0\n" "__abs__" nil
                        ("Special methods")
                        nil "/Users/yojan/.emacs.d/snippets/python-mode/__abs__" nil nil)))


;;; Do not edit! File generated at Thu Nov 20 13:20:18 2025
