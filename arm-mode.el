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
  (list
   '("\\<\\(A\\(?:D\\(?:D\\(?:16\\|8\\|SUBX\\)\\|[CD]\\)\\|ND\\)\\|B\\(?:IC\\|KPT\\|LX\\|XJ\\|[LX]\\)\\|C\\(?:DP2?\\|LZ\\|M[NP]\\|P\\(?:SI[DE]\\|[SY]\\)\\)\\|EOR\\|LD\\(?:C2\\|REX\\|[CMR]\\)\\|M\\(?:AR\\|CR\\(?:R2\\|[2R]\\)?\\|IA\\(?:PH\\|XY\\)?\\|LA\\|OV\\|R\\(?:C2\\|RC2?\\|[ACS]\\)\\|SR\\|UL\\|VN\\)\\|NOP\\|ORR\\|PKH\\(?:BT\\|TB\\)\\|Q\\(?:ADD\\|D\\(?:ADD\\|SUB\\)\\|SUB\\)\\|R\\(?:EV\\(?:16\\|SH\\)?\\|FE\\|S[BC]\\)\\|S\\(?:BC\\|EL\\|M\\(?:L\\(?:A\\(?:L[DX]\\|[WX]Y\\|[DL]\\)\\|S\\(?:L?D\\)\\)\\|M\\(?:L[AS]\\|UL\\)\\|U\\(?:AD\\|L\\(?:L\\|[WX]Y\\)\\|SD\\)\\)\\|RS\\|SAT\\(?:16\\)?\\|T\\(?:C2\\|REX\\|[CMR]\\)\\|UB\\(?:16\\|8\\|ADDX\\)?\\|VC\\|W[IP]\\|XT\\(?:A\\(?:B16\\|[BH]\\)\\|B16\\|[BH]\\)\\)\\|T\\(?:EQ\\|ST\\)\\|U\\(?:M\\(?:\\(?:AA\\|LA\\|UL\\)L\\)\\|SA\\(?:D\\(?:A?8\\)\\|T\\(?:16\\)?\\)\\|XT\\(?:A\\(?:B16\\|[BH]\\)\\|B16\\|[BH]\\)\\)\\|a\\(?:d\\(?:d\\(?:16\\|8\\|subx\\)\\|[cd]\\)\\|nd\\)\\|b\\(?:ic\\|kpt\\|lx\\|xj\\|[lx]\\)\\|c\\(?:dp2?\\|lz\\|m[np]\\|p\\(?:si[de]\\|[sy]\\)\\)\\|eor\\|ld\\(?:c2\\|rex\\|[cmr]\\)\\|m\\(?:ar\\|cr\\(?:r2\\|[2r]\\)?\\|ia\\(?:ph\\|xy\\)?\\|la\\|ov\\|r\\(?:c2\\|rc2?\\|[acs]\\)\\|sr\\|ul\\|vn\\)\\|nop\\|orr\\|pkh\\(?:bt\\|tb\\)\\|q\\(?:add\\|d\\(?:add\\|sub\\)\\|sub\\)\\|r\\(?:ev\\(?:16\\|sh\\)?\\|fe\\|s[bc]\\)\\|s\\(?:bc\\|el\\|m\\(?:l\\(?:a\\(?:l[dx]\\|[wx]y\\|[dl]\\)\\|s\\(?:l?d\\)\\)\\|m\\(?:l[as]\\|ul\\)\\|u\\(?:ad\\|l\\(?:l\\|[wx]y\\)\\|sd\\)\\)\\|rs\\|sat\\(?:16\\)?\\|t\\(?:c2\\|rex\\|[cmr]\\)\\|ub\\(?:16\\|8\\|addx\\)?\\|vc\\|w[ip]\\|xt\\(?:a\\(?:b16\\|[bh]\\)\\|b16\\|[bh]\\)\\)\\|t\\(?:eq\\|st\\)\\|u\\(?:m\\(?:\\(?:aa\\|la\\|ul\\)l\\)\\|sa\\(?:d\\(?:a?8\\)\\|t\\(?:16\\)?\\)\\|xt\\(?:a\\(?:b16\\|[bh]\\)\\|b16\\|[bh]\\)\\)\\|[Bb]\\)\\>"
     . font-lock-keyword-face)		;instrctions
   '("^\\s-*\\.[a-zA-Z]+" . font-lock-keyword-face) ;.data, .text .global, etc
   '("\\(?:\\b\\|\\_>\\)\\s-+\\.[a-zA-Z]+" . font-lock-type-face) ;data types
   '("^\\(.*?\\):\\(.*\\)" 1 font-lock-function-name-face)) ;labels
  "Lowest level of syntax highlighting: keywords and labels.")
(defconst arm-font-lock-keywords-2
  (append arm-font-lock-keywords-1
	  (list
	   '("\\<\\(r\\|w\\|x\\)\\(?:3[0-1]\\|[1-2][0-9]\\|[0-9]\\)\\>"
	     . font-lock-variable-name-face) ;registers
	   '("\\<\\(e?lr\\|pc\\|w?sp\\)\\>" . font-lock-builtin-face) ;special registers
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
