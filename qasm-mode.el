;; qasm-mode.el

;; Author: Joshua Larkin
;; Github: j0shualarkin

;; Emacs Mode for QASM
;; QASM is Quantum Assembly Language

;; working off of this link:
;; https://www.emacswiki.org/emacs/ModeTutorial

;; ------------------

(defvar qasm-mode-hook nil)
;; qasm-mode-hook allows users to run their own code when qasm-mode is run

;; ------------------

(defvar qasm-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for QASM major mode")

;; mode-map allows users to define their own keymaps
;; set to default keymap,
;; the added key ("\C-j") is redundant (happens by default)
;; but is left as an example 

;; ------------------

;;; ###autoload
;; whenever a .qasm file is opened with emacs, qasm-mode starts in that buffer 
(add-to-list 'auto-mode-alist '("\\.qasm\\'" . qasm-mode))

;; ------------------

(defconst qasm-font-lock-keywords-1
  (list
   '("\\<\\(?:->\\|CX\\|U\\|barrier\\|creg\\|gate\\(?:name\\)?\\|if\\|measure\\|opaque\\|qreg\\|reset\\)\\>" . font-lock-builtin-face)
   '("\\('\\w*'\\)" . font-lock-variable-name-face))
  "Basic highlighting expressions for QASM mode")

;; strings used to generate optimized regexp above
;; (regexp-opt '("creg" "qreg" "barrier" "if"
;;               "include" "gate" "opaque"
;;               "U" "CX" "measure" "reset"
;;               "gatename" "->"))

(defconst qasm-font-lock-keywords-2
  (append qasm-font-lock-keywords-1
	  (list
	   '("\\<\\(?:OPENQASM\\|include\\)\\>" . font-lock-keyword-face)))
  "additional keywords to highlight in qasm-mode")

(defvar qasm-font-lock-keywords
  qasm-font-lock-keywords-2
  "Default syntax highlighting for qasm-mode")

;; ------------------

(defconst qasm-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/  ". 12b" table)
    (modify-syntax-entry ?\n "> b"   table)
    table)
  "Syntax table for qasm-mode")

;; ------------------

(defun qasm-mode ()
  "Major mode for editing OPENQASM files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table qasm-mode-syntax-table)
  (use-local-map qasm-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(qasm-font-lock-keywords))
  ;; (set (make-local-variable 'indent-line-function) 'qasm-indent-line)
  (setq major-mode 'qasm-mode)
  (setq mode-name "QASM")
  (run-hooks 'qasm-mode-hook))

(provide 'qasm-mode)
