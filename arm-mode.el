(defvar arm-mode-hook nil
  "Hook for ARMv8 major mode")
(defvar arm-mode-map
  (let ((map (make-keymap)))
	)
  "Keymap for ARMv8 major mode")
;;;;###autoload
(add-to-list 'auto-mode-alist '("\\.arm\\'" . arm-mode))

;;;; font-lock, syntax highlighting
(defconst arm-font-lock-keywords-1
  (list
   '("\\<\\(A\\(?:D\\(?:D\\(?:16\\|8\\|SUBX\\)\\|[CD]\\)\\|ND\\)\\|B\\(?:IC\\|KPT\\|LX\\|XJ\\|[LX]\\)\\|C\\(?:DP2?\\|LZ\\|M[NP]\\|P\\(?:SI[DE]\\|[SY]\\)\\)\\|EOR\\|LD\\(?:C2\\|REX\\|[CMR]\\)\\|M\\(?:AR\\|CR\\(?:R2\\|[2R]\\)?\\|IA\\(?:PH\\|XY\\)?\\|LA\\|OV\\|R\\(?:C2\\|RC2?\\|[ACS]\\)\\|SR\\|UL\\|VN\\)\\|NOP\\|ORR\\|PKH\\(?:BT\\|TB\\)\\|Q\\(?:ADD\\|D\\(?:ADD\\|SUB\\)\\|SUB\\)\\|R\\(?:EV\\(?:16\\|SH\\)?\\|FE\\|S[BC]\\)\\|S\\(?:BC\\|EL\\|M\\(?:L\\(?:A\\(?:L[DX]\\|[WX]Y\\|[DL]\\)\\|S\\(?:L?D\\)\\)\\|M\\(?:L[AS]\\|UL\\)\\|U\\(?:AD\\|L\\(?:L\\|[WX]Y\\)\\|SD\\)\\)\\|RS\\|SAT\\(?:16\\)?\\|T\\(?:C2\\|REX\\|[CMR]\\)\\|UB\\(?:16\\|8\\|ADDX\\)?\\|W[IP]\\|XT\\(?:A\\(?:B16\\|[BH]\\)\\|B16\\|[BH]\\)\\)\\|T\\(?:EQ\\|ST\\)\\|U\\(?:M\\(?:\\(?:AA\\|LA\\|UL\\)L\\)\\|SA\\(?:D\\(?:A?8\\)\\|T\\(?:16\\)?\\)\\|XT\\(?:A\\(?:B16\\|[BH]\\)\\|B16\\|[BH]\\)\\)\\|a\\(?:d\\(?:d\\(?:16\\|8\\|subx\\)\\|[cd]\\)\\|nd\\)\\|b\\(?:ic\\|kpt\\|lx\\|xj\\|[lx]\\)\\|c\\(?:dp2?\\|lz\\|m[np]\\|p\\(?:si[de]\\|[sy]\\)\\)\\|eor\\|ld\\(?:c2\\|rex\\|[cmr]\\)\\|m\\(?:ar\\|cr\\(?:r2\\|[2r]\\)?\\|ia\\(?:ph\\|xy\\)?\\|la\\|ov\\|r\\(?:c2\\|rc2?\\|[acs]\\)\\|sr\\|ul\\|vn\\)\\|nop\\|orr\\|pkh\\(?:bt\\|tb\\)\\|q\\(?:add\\|d\\(?:add\\|sub\\)\\|sub\\)\\|r\\(?:ev\\(?:16\\|sh\\)?\\|fe\\|s[bc]\\)\\|s\\(?:bc\\|el\\|m\\(?:l\\(?:a\\(?:l[dx]\\|[wx]y\\|[dl]\\)\\|s\\(?:l?d\\)\\)\\|m\\(?:l[as]\\|ul\\)\\|u\\(?:ad\\|l\\(?:l\\|[wx]y\\)\\|sd\\)\\)\\|rs\\|sat\\(?:16\\)?\\|t\\(?:c2\\|rex\\|[cmr]\\)\\|ub\\(?:16\\|8\\|addx\\)?\\|w[ip]\\|xt\\(?:a\\(?:b16\\|[bh]\\)\\|b16\\|[bh]\\)\\)\\|t\\(?:eq\\|st\\)\\|u\\(?:m\\(?:\\(?:aa\\|la\\|ul\\)l\\)\\|sa\\(?:d\\(?:a?8\\)\\|t\\(?:16\\)?\\)\\|xt\\(?:a\\(?:b16\\|[bh]\\)\\|b16\\|[bh]\\)\\)\\|[Bb]\\)\\>"
     . font-lock-keyword-face)
   '("\\(?: \\|^\\)\\.[a-zA-Z]+" . font-lock-keyword-face)
   '("^\\<[^:]*\\>" . font-lock-function-name-face))
  "Lowest level of syntax highlighting: keywords and labels")

(defconst arm-font-lock-keywords-2
  (append arm-font-lock-keywords-1
		  (list
		   '("\\<\\(r\\(?:1[0-9]\\|2[0-9]\\|3[01]\\|[1-9]\\)\\|w\\(?:1[0-9]\\|2[0-9]\\|3[01]\\|[1-9]\\)\\|x\\(?:1[0-9]\\|2[0-9]\\|3[01]\\|[1-9]\\)\\)\\>"
		     . font-lock-variable-name-face)
		   '("\\<\\(e?lr\\|pc\\|w?sp\\)\\>" . font-lock-builtin-face)
		   '("\\<\\([wx]zr\\)\\>" . font-lock-constant-face)))
  "Second level of syntax highlighting: keywords, labels, and registers")
(defconst arm-font-lock-keywords-3
  (append arm-font-lock-keywords-2
		  (list
		   '("0x[a-fA-f0-9]+" . font-lock-type-face)))
  "Third level of syntax highlighting. keywords, labels, registers, and hexidecimal numbers")
(defvar arm-font-lock-keywords arm-font-lock-keywords-3
  "Default syntax highlighting: keywords, labels, registers, and hexidecimal numbers")

;;;; syntax table
(defvar arm-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?: "." st)
    (modify-syntax-entry ?. "." st)
    ;; comments
    (modify-syntax-entry ?/ ". 14" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?@ "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax tables for arm-mode")

;; entry function
(defun arm-mode ()
  "Major mode for editing Advanced RISC Machine language files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table arm-mode-syntax-table)
  (use-local-map arm-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(arm-font-lock-keywords))
  ;; (set (make-local-variable 'indent-line-function) 'arm-indent-line)
  (setq major-mode 'arm-mode)
  (setq mode-name "ARM Assembly")
  (run-hooks 'prog-mode-hook)
  (run-hooks 'arm-mode-hook))

(provide 'arm-mode)
