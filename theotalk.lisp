;; main.lisp
(in-package :theotalk)

(load "lexer.lisp")   ;; Lexer file
(load "parser.lisp")   ;; Parser file
(load "evaluator.lisp") ;; Evaluator file

(defun run-program-from-file (file-name)
  (with-open-file (stream file-name)
    (let ((input (make-string (file-length stream) :initial-element #\Space)))
      (read-sequence input stream)
      (let ((tokens (tokenize input)))       ;; Tokenize the input
        (let ((ast (parse tokens)))           ;; Parse the tokens into an AST
          (eval ast))))))                     ;; Evaluate the AST
