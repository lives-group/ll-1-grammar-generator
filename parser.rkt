#lang racket
 
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))
(require parser-tools/yacc)
(require "glc-gen.rkt")

(define-tokens value-tokens (NUMBER))
(define-empty-tokens op-tokens (EOF ADD SUBTRACT PRODUCT DIVISION POWER NEG))
 
(define next-token
  (lexer-src-pos
   [(eof) (token-EOF)]
   [(:+ whitespace) (return-without-pos (next-token input-port))]
   [#\+ (token-ADD)]
   [#\- (token-SUBTRACT)]
   [#\* (token-PRODUCT)]
   [#\/ (token-DIVISION)]      
   [#\^ (token-POWER)]
   [#\n (token-NEG)]
   [(:: (:+ numeric) (:* (:: #\. (:+ numeric) ))) (token-NUMBER (string->number lexeme))]))

(define myparser
  (parser
 
   (start exp)
   (end EOF)
   (tokens value-tokens op-tokens )
   (src-pos)
   (error (lambda (a b c d e) (begin (printf "a = ~a\nb = ~a\nc = ~a\nd = ~a\ne = ~a\n" a b c d e) (void))))   
   
   (grammar
 
    (exp  [(NUMBER) $1]
          [(exp exp ADD) (+ $1 $2)]
          [(exp exp SUBTRACT) (- $1 $2)]
          [(exp exp PRODUCT) (* $1 $2)]
          [(exp exp DIVISION) (/ $1 $2)]
          [(exp exp POWER) (expt $1 $2)]
          [(exp NEG) (- $1)]))))

(define (parse ip)
  (port-count-lines! ip)  
  (myparser (lambda () (next-token ip))))   
 
(parse (open-input-string "20 3 5 * 7 + + "))




;;;;

;(define-empty-tokens ignore-tokens (EOF))

(define (create-token-definition Σ)
  `(define-tokens alphabet-tokens ,Σ))

(define (create-lexer-rules Σ)
  (map (lambda (sym)
         `([,(symbol->string sym) ,(string->symbol (format "'~a" sym))]))
      Σ))

(define (create-next-token Σ)
  (define lexer-rules
    (append (create-lexer-rules Σ)
            '([(eof) (token-EOF)])))
  `(define (next-token lexer-src-pos1)
     (lexer-src-pos1
      ,@lexer-rules)))

#;(define (create-parser Σ V productions)
  (define (create-grammar-rhs rhs)
    ()) ; TODO
  (define (create-grammar-rules productions)
    (map (lambda (production)
           (match production [(Production (NT l) r) `(,l (create-grammar-rhs r))])
           production)))
  (define grammar-rules (create-grammar-rules productions))
  `(define custom-parser
     (parser
      (start ,(car (reverse V)) ; TODO: arrumar pra não sair invertido.
      (end EOF)
      (tokens alphabet-tokens op-tokens)
      (src-pos1)
      (error (lambda (a b c d e) (begin (printf "a = ~a\nb = ~a\nc = ~a\nd = ~a\ne = ~a\n" a b c d e) (void))))   
      (grammar
       ,@grammar-rules)))))

(define (teste-cabral Σ V)
  (create-token-definition Σ)
  (create-next-token Σ))