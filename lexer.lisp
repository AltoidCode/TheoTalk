(defun tokenize (input)
  (let ((tokens '())
        (current ""))
    (dolist (char (coerce input 'list) tokens)
      (cond
        ;; Handle whitespace
        ((find char " \t\n")
         (unless (zerop (length current))
           (push current tokens)
           (setf current "")))
        
        ;; Handle digits and numbers
        ((find char "0123456789")
         (setf current (concatenate 'string current (string char))))
        
        ;; Handle strings (quoted text)
        ((char= char #\')  ; Single quote for strings
         (setf current (concatenate 'string current (string char))))
        
        ;; Handle keywords and identifiers
        ((find char "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
         (setf current (concatenate 'string current (string char))))
        
        ;; Handle parentheses, brackets, and operators
        ((char= char #\() (push "(" tokens))
        ((char= char #\)) (push ")" tokens))
        ((char= char #\[) (push "[" tokens))
        ((char= char #\]) (push "]" tokens))
        ((char= char #\=) (push "=" tokens))
        ((char= char #\+) (push "+" tokens))
        ((char= char #\-) (push "-" tokens))
        ((char= char #\*) (push "*" tokens))
        ((char= char #\/) (push "/" tokens))
        ((char= char #\.) (push "." tokens))
        
        ;; Handle function-like declarations (e.g., int(10), string('Hello'))
        ((char= char #\() (push "(" tokens))
        
        ;; If the character is part of a number or operator
        ((find char "+-*/=><.")
         (push (string char) tokens))
        
        ;; Handle unknown characters
        (t (error "Unrecognized character: ~A" char)))))
  
  (reverse tokens))  ; Return tokens in the correct order
