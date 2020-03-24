(defvar bf-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "." table)
    table))

;;;###autoload
(define-derived-mode brainfuck-mode prog-mode "Brainfuck"
  :syntax-table bf-syntax-table
  (bf--add-keywords)
  (bf--help-doc-fun))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bf" . brainfuck-mode))

(defvar brainfuck-mode-map nil "Keymap for brainfuck-mode.")

(defun bf--add-keywords ()
  (font-lock-add-keywords
   nil
   (list (cons (rx (any "[" "]")) font-lock-keyword-face)
         (cons (rx (any ">" "<" "+" "-" "." ",")) font-lock-function-name-face)
         (cons (rx (not (any "[" "]" ">" "<" "+" "-" "." ","))) font-lock-comment-face))))

(defun bf--help-sym-called-at-point ()
  (unless (eobp)
    (buffer-substring-no-properties (point) (1+ (point)))))

(defun bf--help-lookup-doc (sym)
  "Return document string for SYM."
  (pcase sym
    (">" "Increment the pointer.")
    ("<" "Decrement the pointer.")
    ("+" "Increment the value indicated by the pointer.")
    ("-" "Decrement the value indicated by the pointer.")
    ("." "Print the value indicated by the pointer.")
    ("," "Read one byte from input and store it in the indicated value.")
    ("[" "Jump to the matching `]' if the indicated value is zero.")
    ("]" "Jump to the matching `[' if the indicated value is not zero.")))

(defun bf--help-summerize-doc (sym doc)
  (concat sym " : " (car (split-string doc "[\n\r]+"))))

(defun bf-help-minibuffer-help-string ()
  (interactive)
  (let* ((sym (bf--help-sym-called-at-point))
         (doc (when sym (bf--help-lookup-doc sym))))
    (when doc (bf--help-summerize-doc sym doc))))

(defun bf--help-doc-fun ()
  (make-local-variable 'eldoc-documentation-function)
  (setq eldoc-documentation-function
        'bf-help-minibuffer-help-string))

(defun bf-interpreter (input)
  (interactive)
  (let* ((input-list (-map #'char-to-string (coerce input 'list)))
         (ptr 0)
         (mem (make-vector 30000 0))
         (braces (make-vector (length input-list) 0))
         (braces-stack '()))
    (dotimes (outer (length input-list))
      (if (string-equal (nth outer input-list) "[")
          (let ((cnt 0))
            (progn
              (do ((inner 0 (1+ inner)))
                  ((< (length (nthcdr outer input-list)) inner))
                (cond ((string-equal (nth (+ outer inner) input-list) "[") (push t braces-stack))
                      ((string-equal (nth (+ outer inner) input-list) "]") (pop braces-stack)))
                (if (zerop (length braces-stack))
                    (setq inner (length (nthcdr outer input-list)))
                    (incf cnt)))
              (aset braces outer (+ outer cnt))
              (aset braces (+ outer cnt) outer)))))
    (do ((index 0 (1+ index)))
        ((< (length input-list) index))
      (pcase (nth index input-list)
        (">" (incf ptr))
        ("<" (decf ptr))
        ("+" (aset mem ptr (incf (aref mem ptr))))
        ("-" (aset mem ptr (decf (aref mem ptr))))
        ("." (princ (char-to-string (aref mem ptr))))
        ("," (aset mem ptr (read-char)))
        ("[" (if (zerop (aref mem ptr)) (setq index (incf (aref braces index)))))
        ("]" (unless (zerop (aref mem ptr)) (setq index (aref braces index))))))))

(defun bf-exec ()
  (interactive)
  (let ((str (buffer-string)))
    (bf-interpreter str)))

(provide 'brainfuck-mode)
