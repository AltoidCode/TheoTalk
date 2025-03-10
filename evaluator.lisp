(defun theotalk-eval (ast)
  (cond
    ((eq (car ast) 'consolePrint) (theotalk-eval-consolePrint (cdr ast)))
    ((eq (car ast) 'let) (theotalk-eval-let (cdr ast)))
    ((eq (car ast) 'if) (theotalk-eval-if (cdr ast)))
    ((eq (car ast) 'function) (theotalk-eval-function (cdr ast)))
    ;; Add more cases as necessary
    (t (error "Unknown AST node" ast))))

(defun theotalk-eval-consolePrint (ast)
  (format t "~A~%" (eval (first ast))))

(defun theotalk-eval-if (ast)
  (let ((condition (eval (first ast)))
        (true-branch (second ast))
        (false-branch (third ast)))
    (if condition
        (theotalk-eval true-branch)
        (when false-branch
          (theotalk-eval false-branch)))))

(defun theotalk-eval-let (ast)
  ;; Handle variable assignments and evaluations
  (let ((var (first ast))
        (type (second ast))
        (value (third ast)))
    (setf (symbol-value var) value)))

(defun theotalk-eval-function (ast)
  ;; Evaluate function calls
  (let ((name (first ast))
        (params (second ast))
        (body (third ast)))
    (theotalk-eval body)))
