;;; apex-mode.el --- cc-mode language file for salesforce Apex

;; I borrowed some code from emacs cc-mode an quickly put this together.
;; Copyright (C) Free Software Foundation, Inc.

;; This file is not part of GNU Emacs.

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

;; This file contains definitions to parse salesforce Apex code

;;; Code:

(require 'cc-mode)
(require 'cc-mode)
(require 'cc-fonts)
(require 'cc-langs)
(require 'cc-bytecomp)
(require 'compile)

(add-to-list 'auto-mode-alist '("\\.cls\\'" . apex-mode))
(add-to-list 'auto-mode-alist '("\\.trigger\\'" . apex-mode))
 
;; The language constants are needed when compiling.
(eval-when-compile
  (let ((load-path
         (if (and (boundp 'byte-compile-dest-file)
                  (stringp byte-compile-dest-file))
             (cons (file-name-directory byte-compile-dest-file) load-path)
           load-path)))
    (load "cc-mode" nil t)
    (load "cc-fonts" nil t)
    (load "cc-langs" nil t)
    (load "cc-bytecomp" nil t)))

(eval-and-compile
  ;; Make our mode known to the language constant system.  Use Java
  ;; mode as the fallback for the constants we don't change here.
  ;; This needs to be done also at compile time since the language
  ;; constants are evaluated then.
  (c-add-language 'apex-mode 'java-mode))

(setq c-basic-offset 4)
(setq indent-tabs-mode nil)

(c-lang-defconst c-make-mode-syntax-table
  "Functions that generates the mode specific syntax tables.
The syntax tables aren't stored directly since they're quite large."
  t `(lambda ()
       (let ((table (make-syntax-table)))
	 (c-populate-syntax-table table)
	 ;; Mode specific syntaxes.
	 ,(cond ((or (c-major-mode-is 'objc-mode) (c-major-mode-is 'java-mode) (c-major-mode-is 'apex-mode))
		 ;; Let '@' be part of symbols in ObjC to cope with
		 ;; its compiler directives as single keyword tokens.
		 ;; This is then necessary since it's assumed that
		 ;; every keyword is a single symbol.
		 `(modify-syntax-entry ?@ "_" table))
		((c-major-mode-is 'pike-mode)
		 `(modify-syntax-entry ?@ "." table)))
	 table)))

;; fix single quote issue introduced with emacs 26:
(c-lang-defconst c-before-font-lock-functions
  apex '(c-depropertize-new-text
         c-restore-<>-properties
         c-change-expand-fl-region))

(c-lang-defconst c-operators
  apex `(
	 ;; Primary.
	 ,@(c-lang-const c-identifier-ops)

	 (postfix-if-paren "<" ">") ; Templates.

	 (prefix "super")

	 (left-assoc ".")

	 (postfix "++" "--" "[" "]" "(" ")")

	 ;; Unary.
	 (prefix "++" "--" "+" "-" "!" "~" "new"
		 "(" ")")			; Cast.

	 ;; Member selection.
	 ,@(when (c-major-mode-is 'c++-mode)
	     `((left-assoc ".*" "->*")))

	 ;; Multiplicative.
	 (left-assoc "*" "/" "%")

	 ;; Additive.
	 (left-assoc "+" "-")

	 ;; Shift.
	 (left-assoc "<<" ">>" ">>>")

	 ;; Relational.
	 (left-assoc "<" ">" "<=" ">=" "insanceof")

	 ;; Equality.
	 (left-assoc "==" "!=" "!==" "===" "!==")

	 ;; Bitwise and.
	 (left-assoc "&")

	 ;; Bitwise exclusive or.
	 (left-assoc "^")

	 ;; Bitwise or.
	 (left-assoc "|")

	 ;; Logical and.
	 (left-assoc "&&")

	 ;; Logical or.
	 (left-assoc "||")

	 ;; Conditional.
	 (right-assoc-sequence "?" ":")

	 ;; Assignment.
	 (right-assoc ,@(c-lang-const c-assignment-operators))

	 ;; Sequence.
	 (left-assoc ",")))


;; helper functions for constructing case-insensitive regex
(defun anycase-letter (letter inside)
  (if (string-match "[[:alpha:]]" letter)
      (if inside
          (concat (downcase letter) (upcase letter))
        (concat "[" (downcase letter) (upcase letter) "]"))
    letter))


(defun anycase-regexp (source)
  (let ((result)
        (brace-lvl 0))
    (dotimes (i (length source))
      (let* ((current (substring source i (+ i 1)))
             (previous (when (> i 0) (substring source (- i 1) i)))
             (notspecial (not (string= "\\" previous))))
        (if notspecial
            (progn
              (when (string= "[" current) (setq brace-lvl (+ brace-lvl 1)))
              (when (string= "]" current) (setq brace-lvl (- brace-lvl 1)))
              (setq result (concat result (anycase-letter current (> brace-lvl 0))))) ;; FIXME: should probably not do this for [[?]]
          (setq result (concat result current)))))
    result))


;; overwrite the regex function and modify for case insensitive matching
(defun c-make-keywords-re (adorn list &optional mode)

  ;; delete duplicates
  (let (unique)
    (dolist (elt list)
      (unless (member elt unique)
	(push elt unique)))
    (setq list (delete nil unique)))


  (if list
      (let (re)

  ;; This is kludgy but it works: Search for a string that
  ;; doesn't occur in any word in LIST.  Append it to all
  ;; the alternatives where we want to add \>.  Run through
  ;; `regexp-opt' and then replace it with \>.
	(if (eq adorn 'appendable)
	    (let ((unique "") pos)
	      (while (let (found)
		       (setq unique (concat unique "@")
			     pos list)
		       (while (and pos
				   (if (string-match unique (car pos))
				       (progn (setq found t)
					      nil)
				     t))
			 (setq pos (cdr pos)))
		       found))
	      (setq pos list)
	      (while pos
		(if (string-match "\\w\\'" (car pos))
		    (setcar pos (concat (car pos) unique)))
		(setq pos (cdr pos)))
	      (setq re (regexp-opt list))
	      (setq pos 0)
	      (while (string-match unique re pos)
		(setq pos (+ (match-beginning 0) 2)
		      re (replace-match "\\>" t t re))))
	  (if (c-major-mode-is 'apex-mode) 
	      (setq re (anycase-regexp (regexp-opt list))) ;; make case insensitive!!
	    (setq re (regexp-opt list))))

	;; Emacs 20 and XEmacs (all versions so far) has a buggy
	;; regexp-opt that doesn't always cope with strings containing
	;; newlines.  This kludge doesn't handle shy parens correctly
	;; so we can't advice regexp-opt directly with it.
	(let (fail-list)
	  (while list
	    (and (string-match "\n" (car list)) ; To speed it up a little.
		 (not (string-match (concat "\\`\\(" re "\\)\\'")
				    (car list)))
		 (setq fail-list (cons (car list) fail-list)))
	    (setq list (cdr list)))
	  (when fail-list
	    (setq re (concat re
			     "\\|"
			     (mapconcat
			      (if (eq adorn 'appendable)
				  (lambda (str)
				    (if (string-match "\\w\\'" str)
					(concat (regexp-quote str)
						"\\>")
				      (regexp-quote str)))
				'regexp-quote)
			      (sort fail-list
				    (lambda (a b)
				      (> (length a) (length b))))
			      "\\|")))))

	;; Add our own grouping parenthesis around re instead of
	;; passing adorn to `regexp-opt', since in XEmacs it makes the
	;; top level grouping "shy".
	(cond ((eq adorn 'appendable)
	       (concat "\\(" re "\\)"))
	      (adorn
	       (concat "\\(" re "\\)"
		       "\\("
		       (c-get-lang-constant 'c-nonsymbol-key nil mode)
		       "\\|$\\)"))
	      (t
	       re)))

    ;; Produce a regexp that matches nothing.
    (if adorn
	"\\(\\<\\>\\)"
      "\\<\\>")))

(put 'c-make-keywords-re 'lisp-indent-function 1)

;;;
(c-lang-defconst c-decl-block-key
  apex "\\([cC][lL][aA][sS][sS]\\|[iI][nN][tT][eE][rR][fF][aA][cC][eE]\\|[tT][rR][iI][gG][gG][eE][rR]\\)\\([^[:alnum:]_]\\|$\\)")

;; Apex does generics.  Setting this to t tells the parser to put
;; parenthesis syntax on angle braces that surround a comma-separated
;; list.
(c-lang-defconst c-recognize-<>-arglists
  apex t)

;;;

(c-lang-defconst c-assignment-operators
  apex '("=" "+=" "*=" "-=" "/=" "|=" "&=" "^=" "<<=" ">>=" ">>>="))

(c-lang-defconst c-doc-comment-start-regexp
  apex "\\<\\>")

(c-lang-defconst c-paragraph-start
  apex "$")

;; Primitive types
(c-lang-defconst c-primitive-type-kwds
  apex '("Blob" "Boolean" "String" "Object" "Date" "DateTime" "Decimal" "Double" "Id" "Integer" "Long" "Time"))

(c-lang-defconst c-class-decl-kwds
  apex '("class" "interface" "trigger"))

;; The various modifiers.
(c-lang-defconst c-modifier-kwds
                 apex '("abstract" "final" "private" "protected" "public" "global" "static" "override" "transient"
                        "testmethod" "virtual" "webservice" "with sharing" "without sharing"))

(c-lang-defconst c-other-decl-kwds
  apex nil)

(c-lang-defconst c-postfix-decl-spec-kwds
  apex '("extends" "implements"))

(c-lang-defconst c-type-list-kwds
  apex '("new" "extends" "super" "implements" "on"))

(c-lang-defconst c-ref-list-kwds
  apex nil)


(c-lang-defconst c-brace-list-decl-kwds
  apex '("enum" "new"))

(c-lang-defconst c-inexpr-brace-list-kwds
  apex '("new"))


(c-lang-defconst c-block-stmt-2-kwds
  apex '("for" "if" "while" "catch"))

(c-lang-defconst c-simple-stmt-kwds
  apex '("break" "continue" "return" "throw" "insert" "update" "upsert" "delete" "undelete" "merge"))

(c-lang-defconst c-case-kwds
  apex nil)

(c-lang-defconst c-label-kwds
  apex nil)

(c-lang-defconst c-before-label-kwds
  apex nil)

; Constants
(c-lang-defconst c-constant-kwds 
  apex '("true" "false" "null"))

(c-lang-defconst c-other-kwds
  apex '("and" "as" "asc" "bulk" "by" "desc" "from" "instanceof" "like" "limit" "not" "nulls" "on" "or" "select" "where"))

;;;
;;;


(defconst apex-font-lock-keywords-1 (c-lang-const c-matchers-1 apex)
  "Minimal highlighting for Apex mode.")

(defconst apex-font-lock-keywords-2 (c-lang-const c-matchers-2 apex)
  "Fast normal highlighting for Apex mode.")

(defconst apex-font-lock-keywords-3 (c-lang-const c-matchers-3 apex)
  "Accurate normal highlighting for Apex mode.")

(defvar apex-font-lock-keywords apex-font-lock-keywords-3
  "Default expressions to highlight in Apex mode.")

(defvar apex-mode-syntax-table nil
  "Syntax table used in apex-mode buffers.")
(or apex-mode-syntax-table
    (setq apex-mode-syntax-table
	  (funcall (c-lang-const c-make-mode-syntax-table apex))))

(defvar apex-mode-abbrev-table nil
  "Abbreviation table used in apex-mode buffers.")
(c-define-abbrev-table 'apex-mode-abbrev-table
  ;; Keywords that if they occur first on a line might alter the
  ;; syntactic context, and which therefore should trig reindentation
  ;; when they are completed.
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)
    ("finally" "finally" c-electric-continued-statement 0)))

(defvar apex-mode-map (let ((map (c-make-inherited-keymap)))
                           ;; Add bindings which are only useful for Apex
                           map)
  "Keymap used in apex-mode buffers.")

(easy-menu-define apex-menu apex-mode-map "Apex Mode Commands"
  (cons "Apex" (c-lang-const c-mode-menu apex)))

;;; The entry point into the mode
(defun apex-mode ()
  "Major mode for editing Apex code.
This is a simple example of a separate mode derived from CC Mode to
support a language with syntax similar to C/C++/ObjC/Java/IDL/Pike.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `apex-mode-hook'.

Key bindings:
\\{apex-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table apex-mode-syntax-table)
  (setq major-mode 'apex-mode
	mode-name "Apex"
	local-abbrev-table apex-mode-abbrev-table
	abbrev-mode t)
  (use-local-map apex-mode-map)
  ;; `c-init-language-vars' is a macro that is expanded at compile
  ;; time to a large `setq' with all the language variables and their
  ;; customized values for our language.
  (c-init-language-vars apex-mode)
  ;; `c-common-init' initializes most of the components of a CC Mode
  ;; buffer, including setup of the mode menu, font-lock, etc.
  ;; There's also a lower level routine `c-basic-common-init' that
  ;; only makes the necessary initialization to get the syntactic
  ;; analysis and similar things working.
  (c-common-init 'apex-mode)
  (easy-menu-add apex-menu)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'apex-mode-hook)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'arglist-intro '+)
  (c-update-modeline))

(provide 'apex-mode)

;;; apex-mode.el ends here
