(defvar arm-mode-hook nil
  "Hook for ARMv8 major mode.")
(defvar arm-tab-width 4
  "Width of tabs for arm mode.")
(defvar arm-insert-tab nil
  "When t, TAB inserts a tab instead of auto indention the code.
arm-insert-tab should be nil if you want to wrtie old style assembler code.")
(defvar arm-comment-char "@"
  "Character to denote inline comments.")
(defvar arm-mode-map nil
  "Keymap for ARMv8 major mode.")
(setq arm-mode-map (make-sparse-keymap))
(define-key arm-mode-map (kbd "M-;") #'arm-insert-comment)
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.arm\\'" . arm-mode))
     
;;;; font-lock, syntax highlighting
(defconst arm-font-lock-keywords-1
  (eval-when-compile
	(let ((suffixes (regexp-opt '("eq" "ne" "cs" "hs" "cc" "lo" "mi" "pl" "vs" "vc" "hi" "ls" "ge" "lt" "gt" "le" "al" "f32" "f64")))
		  (suffix-instrs (regexp-opt '("add" "adc" "qadd" "qdadd" "sub" "sbc" "rsb" "rsc" "qsub" "qdsub" "mul" "mla" 
									   "smull" "smlal" "smulxy" "smulwy" "smlaxy" "smlawy" "smlalx" "smuad" "smlad" "smlald" "smusd" "smlsd"
									   "smlsld" "smmul" "smmla" "smmls" "mia" "miaph" "miaxy" "clz" "addsubx" "umull" "umlal" "umaal"
									   "subaddx" "usad8" "usada8" "mov" "mvn" "movt" "mrs" "msr" "msr" "mra" "mar" "cpy" "tst" "teq" "and" "eor" "orr"
									   "bic" "cmp" "cmn" "ssat" "ssat" "ssat16" "usat" "usat" "usat16" "pkhbt" "pkhtb" "sxth" "sxtb16" "sxtb"
									   "uxth" "uxtb16" "uxtb" "sxtah" "sxtab16" "sxtab" "uxtah" "uxtab16" "uxtab" "rev" "rev16" "revsh" "sel"
									   "b" "bl" "bx" "bxj" "beq" "bne" "bcs" "bhs" "bcc" "blo" "bmi" "bpl" "bvs" "bvc" "bhi" "bls" "bge"
									   "blt" "bgt" "bfc" "bfi" "sbfx" "ubfx" "ble" "bal" "lsl" "lsr" "asr" "ror" "rrx" "dbg" "sev" "wfe" "wfi"
									   "yield" "crd" "swi" "nop" "ldr" "ldm" "ldrex" "str" "stm" "strex" "swp"  "ldc" "ldc2" "stc" "stc2" "svc"
									   "d" "pop" "push" "strexd" "swpb" "smc"  "subs" "adr")
									 t))
		  (non-suffix-instrs (regexp-opt '("it" "blx" "cb" "tbb" "tbh" "cpsid" "cpsie" "cps" "setend" "clrex" "cdp"
										   "cdp2" "mrc" "mrc2" "mrrc" "mrrc2" "mcr" "mcr2" "mcrr" "mcrr2" "srs" "rfe"
										   "bkpt" "dmb" "dsb" "isb")
										 t))
		  (prefixs (regexp-opt '("s" "q" "sh" "u" "uq" "uh")))
		  (prefix-instrs (regexp-opt '("add16" "sub16" "add8" "sub8" "sax" "asx") t)))
	  (list
	   '("^\\s-*\\.[a-zA-Z]+" . font-lock-keyword-face) ;.data, .text .global, etc
	   '("\\(?:\\b\\|\\_>\\)\\s-+\\.[a-zA-Z]+" . font-lock-type-face) ;data types
	   '("^\\(.*?\\):\\(.*\\)" 1 font-lock-function-name-face) ;labels
	   `(,(concat "\\<v?" suffix-instrs "\\.?" suffixes "?\\>") . font-lock-keyword-face) ;suffix instrctions
	   `(,(concat "\\<v?" prefixs prefix-instrs "\\.?" suffixes "?\\>") . font-lock-keyword-face) ;prefix and suffix  instructions
	   `(,(concat "\\<v?" non-suffix-instrs "\\>") . font-lock-keyword-face)))) ;non suffixs instructions
  "Lowest level of syntax highlighting: keywords and labels.")
(defconst arm-font-lock-keywords-2
  (append (list
		   '("\\<\\(r\\|w\\|x\\|s\\|d\\)\\(?:3[0-1]\\|[1-2][0-9]\\|[0-9]\\)\\>"
			 . font-lock-variable-name-face) ;registers
		   '("\\<\\(e?lr\\|pc\\|w?sp\\|cpsr\\|fpsr\\)\\>" . font-lock-builtin-face) ;special registers
		   '("\\<\\([wx]zr\\)\\>" . font-lock-constant-face)) ;zero registers
		  arm-font-lock-keywords-1)
  "Second level of syntax highlighting: keywords, labels, and registers.")
(defconst arm-font-lock-keywords-3
  (append arm-font-lock-keywords-2
		  (list
		   '("0x[a-fA-f0-9]+" . font-lock-type-face))) ;hexidecimal
  "Third level of syntax highlighting: keywords, labels, registers, and hexidecimal numbers.")
(defvar arm-font-lock-keywords arm-font-lock-keywords-3
  "Default syntax highlighting: keywords, labels, registers, and hexidecimal numbers.")

;;;; syntax table
(defvar arm-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?: "_" st)
	(modify-syntax-entry ?= "_" st)
    (modify-syntax-entry ?. "." st)
	(modify-syntax-entry ?_ "w" st)		;for convention of using _ in labels
    (modify-syntax-entry ?\' "\"" st)
    ;; comments
    (modify-syntax-entry ?/ ". 14" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?@ "< b" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax tables for arm mode.")

;; indentation rules:
;; 1 If we are at the beginning of the buffer, indent to column 0.
;; 2 If the previous line is a non-data label, indent to the right.
;; 3 else indent the same as previous line.
;; 4 if line contains a colon(label), do not change it's indentaion
;; 5 (secret) if the line is a comment allign it to the left
(defun arm-indent-line ()
  "Indent current line of ARM code."
  (interactive)
  (if arm-insert-tab
	  (insert "\t")
	(progn
	  (beginning-of-line)
	  (if (bobp)				;check for rule 1
		  (indent-line-to 0)
		(if (looking-at "^\\s *\\(/\\*\\|@\\)") ;check for rule 5
			(indent-line-to (save-excursion		;indentation of the next line
							  (forward-line 1)
							  (if (looking-at "^$") ;not empty line
								  (progn
									(forward-line -1)
									(current-indentation))
								(current-indentation))))
		  (if (looking-at "^.*:")		;check for rule 4
			  (indent-line-to (current-indentation))
			(let ((not-indented t) cur-indent)
			  (save-excursion
				(while not-indented
				  (forward-line -1)
				  (if (looking-at "^.*:\\s-*\\.") ;data label
					  (progn
						(setq cur-indent (current-indentation))
						(setq not-indented nil))
					(if (looking-at "^.*:") ;check for rule 2
						(progn
						  (setq cur-indent (+ (current-indentation) arm-tab-width))
						  (setq not-indented nil))
					  (if (looking-at "^.[^\\n]")	;check for rule 3
						  (progn
							(setq cur-indent (current-indentation))
							(setq not-indented nil)))))))
			  (if (< cur-indent 0)
				  (setq cur-indent 0))
			  (indent-line-to cur-indent))))))))

(defun arm-insert-comment ()
  "Insert /*   */ if on an empty line.
Then call comment-dwim."
  (interactive)
  (let ((special (and (save-excursion
						(move-beginning-of-line nil)
						(looking-at "^\\s-*$"))
					  (not (use-region-p)))))		;empty line
    (when special
      (progn
		(insert "/*   */")))
    (comment-dwim nil)
    (when special
      (forward-char))))			;move to middle of /*   */

;; entry function
(defun arm-mode ()
  "Major mode for editing Advanced RISC Machine language files."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table arm-mode-syntax-table)
  (use-local-map arm-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(arm-font-lock-keywords nil t))
  (set (make-local-variable 'indent-line-function) #'arm-indent-line)
  (setq major-mode 'arm-mode)
  (setq mode-name "ARM Assembler")
  (run-hooks 'prog-mode-hook)
  (run-hooks 'arm-mode-hook)
  ;; comments
  (setq-local comment-start (concat arm-comment-char " "))
  (setq-local comment-end ""))

(provide 'arm-mode)
