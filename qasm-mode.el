;; qasm-mode.el

;; Author: Joshua Larkin
;; Github: j0shualarkin

;; Emacs Mode for QASM
;; QASM is Quantum Assembly Language

;; ------------------


;; let other emacs functions know what comments are
(defvar qasm-mode-hook
  (add-hook 'qasm-mode-hook
	    (lambda ()
	      (setq comment-start "// "))))

;; ------------------

;; make-template : Number -> String
;; uses an elisp function `insert` to write the string to the current buffer
(defun make-template (n)
    (let ((template (concat "OPENQASM 2.0;\n"
			    "include \"qelib1.inc\";\n\n"
			    (format "qreg in[%d];\n" n)
			    (format "creg out[%d];\n" n)
			    "barrier in;\n"
			    "measure in -> out;")))
      (insert template)))


(defvar qasm-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    (define-key map "\C-c\C-o"
      (defun gen-template (n)
	"Make a basic qasm file set up with a given amount of registers"
	(interactive "nEnter number of registers to put in template: ")
	(make-template n)))
    map)
  "Keymap for QASM major mode")

;; mode-map allows users to define their own keymaps
;; the added key ("\C-j") is redundant (happens by default)
;; but is left as an example 

;; ------------------

;;; ###autoload
;; whenever a .qasm file is opened with emacs, qasm-mode starts in that buffer 
(add-to-list 'auto-mode-alist '("\\.qasm\\'" . qasm-mode))

;; ------------------

(defconst qasm-font-lock-keywords-1
  (list
   (cons (regexp-opt '("creg" "qreg" "barrier" "if"
		       "gate" "opaque" "measure"
		       "reset" "gatename"))
	 font-lock-builtin-face)
   '("\\('\\w*'\\)" . font-lock-variable-name-face))
  "Basic expressions for QASM mode")


(defconst qasm-font-lock-keywords-2
  (append qasm-font-lock-keywords-1
	  (list
	   (cons (regexp-opt '("OPENQASM" "include"))
		 font-lock-keyword-face)))
  "qasm keywords are OPENQASM and include")

(defun my-whitespacer (ls)
  (mapcan (lambda (x)
	    (list (concat x " ")
		  (concat " " x " ")
		  (concat "\n" x " ")
		  (concat "\t" x " ")
		  (concat "\r" x " ")))
	  ls))

(defconst qasm-font-lock-keywords-3
  (append qasm-font-lock-keywords-2
	  (list
	   (cons
	    (let ((strs '("u" "x" "cx" "ccx" "h" "H" "U" "CX" "CCX")))
	      (regexp-opt (my-whitespacer strs)))
	    font-lock-function-name-face)))
  "have the built-in gates as functions")

(defvar qasm-font-lock-keywords
  qasm-font-lock-keywords-3
  ;; use 3 when the right reg-exp is writen
  "Default syntax highlighting for qasm-mode")

;; ------------------

(defconst qasm-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;(modify-syntax-entry ?/  ". 14b" table)
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

  (setq major-mode 'qasm-mode)
  (setq mode-name "QASM")

  (run-hooks 'qasm-mode-hook)
  )


(provide 'qasm-mode)
