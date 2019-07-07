(defvar arm-mode-hook nil
  "Hook for ARMv8 major mode.")
(defvar arm-tab-width 4
  "Width of tabs for arm mode.")
(defvar arm-mode-map nil
  "Keymap for ARMv8 major mode.")
(setq arm-mode-map (make-sparse-keymap))
(define-key arm-mode-map (kbd "M-;") #'arm-insert-comment)
;;;;###autoload
(add-to-list 'auto-mode-alist '("\\.arm\\'" . arm-mode))

;;;; font-lock, syntax highlighting
(defconst arm-font-lock-keywords-1
  (list `(,(eval-when-compile
	  (concat "\\<" (regexp-opt '("add" "adc" "qadd" "qdadd" "sub" "sbc" "rsb" "rsc" "qsub" "qdsub" "mul" "mla" "umull" "umlal" "umaal"
				      "smull" "smlal" "smulxy" "smulwy" "smlaxy" "smlawy" "smlalx" "smuad" "smlad" "smlald" "smusd" "smlsd"
				      "smlsld" "smmul" "smmla" "smmls" "mia" "miaph" "miaxy" "clz" "add16" "sub16" "add8" "sub8" "addsubx"
				      "subaddx" "usad8" "usada8" "mov" "mvn" "mrs" "msr" "msr" "mra" "mar" "cpy" "tst" "teq" "and" "eor" "orr"
				      "bic" "cmp" "cmn" "ssat" "ssat" "ssat16" "usat" "usat" "usat16" "pkhbt" "pkhtb" "sxth" "sxtb16" "sxtb"
				      "uxth" "uxtb16" "uxtb" "sxtah" "sxtab16" "sxtab" "uxtah" "uxtab16" "uxtab" "rev" "rev16" "revsh" "sel"
				      "b" "bl" "bx" "blx" "bxj" "beq" "bne" "bcs" "bhs" "bcc" "blo" "bmi" "bpl" "bvs" "bvc" "bhi" "bls" "bge"
				      "blt" "bgt" "ble" "bal" "lsl" "lsr" "asr" "ror" "rrx" "dbg" "dmb" "dsb" "isb" "sev" "wfe" "wfi" "yield"
				      "crd" "cpsid" "cpsie" "cps" "srs" "rfe" "bkpt" "swi" "nop" "ldr" "ldm" "ldrex" "str" "stm" "strex" "swp"
				      "cdp" "cdp2" "mrc" "mrc2" "mrrc" "mrrc2" "mcr" "mcr2" "mcrr" "mcrr2" "ldc" "ldc2" "stc" "stc2" "svc"
				      "ADD" "ADC" "QADD" "QDADD" "SUB" "SBC" "RSB" "RSC" "QSUB" "QDSUB" "MUL" "MLA" "UMULL" "UMLAL" "UMAAL" "SMULL"
				      "SMLAL" "SMULXY" "SMULWY" "SMLAXY" "SMLAWY" "SMLALX" "SMUAD" "SMLAD" "SMLALD" "SMUSD" "SMLSD" "SMLSLD" "SMMUL"
				      "SMMLA" "SMMLS" "MIA" "MIAPH" "MIAXY" "CLZ" "ADD16" "SUB16" "ADD8" "SUB8" "ADDSUBX" "SUBADDX" "USAD8" "USADA8"
				      "MOV" "MVN" "MRS" "MSR" "MSR" "MRA" "MAR" "CPY" "TST" "TEQ" "AND" "EOR" "ORR" "BIC" "CMP" "CMN" "SSAT" "SSAT"
				      "SSAT16" "USAT" "USAT" "USAT16" "PKHBT" "PKHTB" "SXTH" "SXTB16" "SXTB" "UXTH" "UXTB16" "UXTB" "SXTAH" "SXTAB16"
				      "SXTAB" "UXTAH" "UXTAB16" "UXTAB" "REV" "REV16" "REVSH" "SEL" "B" "BL" "BX" "BLX" "BXJ" "BEQ" "BNE" "BCS" "BHS"
				      "BCC" "BLO" "BMI" "BPL" "BVS" "BVC" "BHI" "BLS" "BGE" "BLT" "BGT" "BLE" "BAL" "LSL" "LSR" "ASR" "ROR" "RRX" "DBG"
				      "DMB" "DSB" "ISB" "SEV" "WFE" "WFI" "YIELD" "CRD" "CPSID" "CPSIE" "CPS" "SRS" "RFE" "BKPT" "SWI" "NOP" "LDR" "LDM"
				      "LDREX" "STR" "STM" "STREX" "SWP" "CDP" "CDP2" "MRC" "MRC2" "MRRC" "MRRC2" "MCR" "MCR2" "MCRR" "MCRR2" "LDC" "LDC2"
				      "STC" "STC2" "SVC") t) "\\>"))
	  . font-lock-keyword-face) ;instrctions
  '("^\\s-*\\.[a-zA-Z]+" . font-lock-keyword-face) ;.data, .text .global, etc
  '("\\(?:\\b\\|\\_>\\)\\s-+\\.[a-zA-Z]+" . font-lock-type-face) ;data types
  '("^\\(.*?\\):\\(.*\\)" 1 font-lock-function-name-face)) ;labels
"Lowest level of syntax highlighting: keywords and labels.")
(defconst arm-font-lock-keywords-2
  (append arm-font-lock-keywords-1
	  (list
	   '("\\<\\(r\\|w\\|x\\)\\(?:3[0-1]\\|[1-2][0-9]\\|[0-9]\\)\\>"
	     . font-lock-variable-name-face) ;registers
	   '("\\<\\(e?lr\\|pc\\|w?sp\\|cpsr\\)\\>" . font-lock-builtin-face) ;special registers
	   '("\\<\\([wx]zr\\)\\>" . font-lock-constant-face))) ;zero registers
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
    (modify-syntax-entry ?. "." st)
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
(defun arm-indent-line ()
  "Indent current line of ARM code."
  (interactive)
  (beginning-of-line)
  (if (bobp)				;check for rule 1
      (indent-line-to 0)
    (if (looking-at "^.*:")		;check for rule 4
	(indent-line-to (current-indentation))
      (let ((not-indented t)
	    cur-indent)
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
	(indent-line-to cur-indent)))))

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
  (set (make-local-variable 'font-lock-defaults) '(arm-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) #'arm-indent-line)
  ;; comments
  (setq-local comment-start "@ ")
  (setq-local comment-end "")
  (setq major-mode 'arm-mode)
  (setq mode-name "ARM Assembler")
  (run-hooks 'prog-mode-hook)
  (run-hooks 'arm-mode-hook))

(provide 'arm-mode)
