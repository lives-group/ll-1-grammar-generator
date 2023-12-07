#lang racket

(require rackcheck)
(require racket/set)
(require "glc-gen.rkt")
(provide (all-defined-out))

;; Constantes.
(define rhs-INVALID #f) ; ∅
(define rhs-EMPTY #t) ; ε

;; Predicados para Regras.
(define (rhs-alt? rhs)
  (and (pair? rhs) (eq? (car rhs) 'alt)))

(define (rhs-seq? rhs)
  (and (pair? rhs) (eq? (car rhs) 'seq)))

(define (rhs-empty? re)
  (eq? re rhs-EMPTY))

(define (rhs-invalid? re)
  (eq? re rhs-INVALID))

(define (rhs-terminal? rhs)
  (rhs-atom? rhs 'terminal))

(define (rhs-non-terminal? rhs)
  (rhs-atom? rhs 'non-terminal))

(define (rhs-atom? rhs head-type)
  (and
   (pair? rhs)
   (eq? (car rhs) head-type)
   (let ([elem (cadr rhs)])
     (or (char? elem) (symbol? elem)))))

;; Funções lambda.
(define (match-seq rhs lambda)
  (and (rhs-seq? rhs)
       (lambda (cadr rhs) (caddr rhs))))

(define (match-alt rhs lambda)
  (and (rhs-alt? rhs)
       (lambda (cadr rhs) (caddr rhs))))

(define (match-terminal rhs lambda)
  (and (rhs-terminal? rhs)
       (lambda (cadr rhs))))

(define (match-non-terminal rhs lambda)
  (and (rhs-non-terminal? rhs)
       (lambda (cadr rhs))))

;; Simplificadores de predicados.
(define (seq rhs1 rhs2)
  (cond
    ((rhs-invalid? rhs1) rhs-INVALID)
    ((rhs-invalid? rhs2) rhs-INVALID)
    ((rhs-empty? rhs1) rhs2)
    ((rhs-empty? rhs2) rhs1)
    (else (list 'seq rhs1 rhs2))))
     
(define (alt rhs1 rhs2)
  (cond
    ((rhs-invalid? rhs1) rhs2)
    ((rhs-invalid? rhs2) rhs1)
    (else (list 'alt rhs1 rhs2))))

(define (terminal rhs)
  (list 'terminal rhs))

(define (non-terminal rhs)
  (list 'non-terminal rhs))

;; Derivada
(define (rhs-empty grammar-hash rhs)
  (cond
    ((rhs-empty? rhs) rhs-EMPTY)
    ((rhs-invalid? rhs) rhs-INVALID)
    ((rhs-terminal? rhs) rhs-INVALID)
    ((rhs-non-terminal? rhs) rhs-INVALID)
    ((match-seq rhs (lambda (rhs1 rhs2)
                     (seq (rhs-empty grammar-hash rhs1) (rhs-empty grammar-hash rhs2)))))
    ((match-alt rhs (lambda (rhs1 rhs2)
                     (alt (rhs-empty grammar-hash rhs1) (rhs-empty grammar-hash rhs2)))))
    (else rhs-INVALID)))

(define (gen:symbol grammar-hash rhs)
  (cond
    ((rhs-empty? rhs) gen:valid)
    ((rhs-invalid? rhs) gen:invalid)
    ((match-terminal rhs (lambda (terminal) (gen:const terminal))))
    ((rhs-non-terminal? rhs) gen:nothing)
    ((match-seq rhs (lambda (rhs1 rhs2)
                      (gen:symbol grammar-hash (if (rhs-empty? rhs1) rhs2 rhs1)))))
    ((match-alt rhs (lambda (rhs1 rhs2)
                      (gen:let ([symbol1 (gen:symbol grammar-hash rhs1)]
                                [symbol2 (gen:symbol grammar-hash rhs2)])
                               (gen:one-of (list symbol1 symbol2))))))
    (else rhs-INVALID)))

(define (gen:grammar-derivate-data grammar-list [depth 1] [max-depth 100])
  (let ([grammar-hash (apply hash (append* grammar-list))]
        [rhs (cadr (car grammar-list))])
    (gen:_grammar-derivate-data grammar-hash rhs (list '()) depth max-depth)))

(define (gen:_grammar-derivate-data grammar-hash rhs entries depth max-depth)
  (let ([derivative-rhs (rhs-derivative grammar-hash rhs (last entries))])
    (cond
      ((> depth max-depth) gen:invalid)
      ((rhs-empty? derivative-rhs) (gen:const entries))
      ((rhs-invalid? derivative-rhs) gen:invalid)
      (else (gen:let ([new-symbol (gen:symbol grammar-hash derivative-rhs)])
                     (if (rhs-empty? new-symbol)
                         (gen:const entries)
                         (gen:_grammar-derivate-data grammar-hash derivative-rhs (flatten (append entries (list new-symbol))) (+ depth 1) max-depth)))))))
    
(define (rhs-derivative grammar-hash rhs symbol)
  (cond 
    ((rhs-empty? rhs) rhs-INVALID)
    ((rhs-invalid? rhs)  rhs-INVALID)
    ((match-terminal rhs (lambda (terminal)
                           (cond
                             ((eq? terminal symbol) rhs-EMPTY)
                             ((empty? symbol) rhs)
                             (else rhs-INVALID)))))
    ((match-non-terminal rhs (lambda (non-terminal)
                           (hash-ref grammar-hash non-terminal))))
    ((match-seq rhs     (lambda (rhs1 rhs2) 
                         (alt (seq (rhs-derivative grammar-hash rhs1 symbol) rhs2)
                              (seq (rhs-empty grammar-hash rhs1) (rhs-derivative grammar-hash rhs2 symbol))))))
    ((match-alt rhs     (lambda (rhs1 rhs2)
                         (alt (rhs-derivative grammar-hash rhs1 symbol) (rhs-derivative grammar-hash rhs2 symbol)))))
    (else rhs-INVALID)
    ))


;; Gerador de entradas
(define (gen:grammar-data grammar-list [depth 1] [max-depth 5])
  (gen:_grammar-data (apply hash (append* grammar-list)) (cadr (car grammar-list)) depth max-depth (set)))

(define (gen:_grammar-data grammar-hash rhs depth max-depth visited-set)
  (cond
    ((rhs-empty? rhs) gen:nothing) 
    ((rhs-terminal? rhs) (gen:terminal rhs))
    ((rhs-non-terminal? rhs) (gen:non-terminal grammar-hash rhs depth max-depth visited-set))
    ((match-seq rhs (lambda (rhs1 rhs2) (gen:seq grammar-hash rhs1 rhs2 depth max-depth visited-set))))
    ((match-alt rhs (lambda (rhs1 rhs2) (gen:alt grammar-hash rhs1 rhs2 depth max-depth visited-set))))
    (else (error "Invalid production."))
    ))

;; Geradores Auxiliares
(define gen:nothing (gen:const null))

(define gen:invalid (gen:const rhs-INVALID))

(define gen:valid (gen:const rhs-EMPTY))

(define (gen:terminal rhs)
  (gen:const (cadr rhs)))

(define (gen:non-terminal grammar-hash rhs depth max-depth visited-set)
  (let ([non-terminal (cadr rhs)]
        [new-depth (if (set-member? visited-set (cadr rhs)) (+ depth 1) depth)])
    (cond
      ((> new-depth max-depth) gen:invalid)
      ((set-member? visited-set non-terminal) (gen:_grammar-data grammar-hash (hash-ref grammar-hash non-terminal) new-depth max-depth (set non-terminal)))
      (else (gen:_grammar-data grammar-hash (hash-ref grammar-hash non-terminal) new-depth max-depth (set-union visited-set (set non-terminal)))))))
  
(define (gen:seq grammar-hash first second depth max-depth visited-set)
  (let ([gen:aux-grammar-data ((curryr gen:_grammar-data) depth max-depth visited-set)])
    (gen:let ([result (gen:tuple (gen:aux-grammar-data grammar-hash first) (gen:aux-grammar-data grammar-hash second))])
             (flatten result))))

(define (gen:alt grammar-hash first second depth max-depth visited-set)
  (let ([gen:aux-grammar-data ((curryr gen:_grammar-data) depth max-depth visited-set)])
    (gen:let ([result-first (gen:aux-grammar-data grammar-hash first)]
              [result-second (gen:aux-grammar-data grammar-hash second)])
             (cond
               ((and (member rhs-INVALID (flatten (list result-first))) (member rhs-INVALID (list result-second))) rhs-INVALID)
               ((member rhs-INVALID (flatten (list result-first))) result-second)
               ((member rhs-INVALID (flatten (list result-second))) result-first)
               (else (gen:one-of (list result-first result-second)))))))

;; INTEGRAÇÃO
(define (transform productions)
  (map transform-ruleset productions))

(define (transform-ruleset production)
  (match production
    [(Production (NT l) r) (list l (transform-ruleset r))]
    [(Seq l r) (seq (transform-ruleset l) (transform-ruleset r))]
    [(Alt l r) (alt (transform-ruleset l) (transform-ruleset r))]
    [(NT x) (non-terminal x)]
    [(T x) (terminal x)]
    ))

;; Check first e follow
(define (assert-ll-1 productions)
  (map (lambda (production)
         (get-first-plus production productions)) productions))

(define (get-first-plus rhs productions)
  (match rhs
    [(Production (NT l) r) (First+ l (get-first-plus r productions))]
    [(Seq l r) (let ([result (get-first-plus l productions)])
                 (if (equal? result rhs-EMPTY) (get-first-plus r productions) result))]
    [(Alt l r) (flatten (list
                         (get-first-plus l productions)
                         (get-first-plus r productions)))] ; TODO1: Remover repetições; Tratar vazio (Follow)
    [(NT x) (if
             (equal? x non-terminal)
             (error "Same non-terminal")
             (get-first-plus (find-production x productions) productions))]
    [(T x) (list x)]
    [else (error "get-first-plus failed!")]
    ))
  

(define (find-production non-terminal productions)
  (findf (lambda (production)
           (match production
             [(Production (NT l) r) (equal? l non-terminal)]
             [else #\f])) productions))
;;-------------------------------------------------------------------------------------------
;; DEBUG
; Ex:
; S -> aAb
; A -> cB
; B -> dC | d
; C -> eB
(define S (seq (terminal 'a) (seq (non-terminal 'A) (terminal 'b))))
(define A (seq (terminal 'c) (non-terminal 'B)))
(define B (alt (seq (terminal 'd) (non-terminal 'C)) (terminal 'd)))
(define C (seq (terminal 'e) (non-terminal 'B)))
(define teste1-hash (hash 'S S 'A A 'B B 'C C))
(define teste1-list (list (list 'S S) (list 'A A) (list 'B B) (list 'C C)))
(define gen:foo (gen:grammar-data teste1-list))
(define gen:bar (gen:grammar-derivate-data teste1-list))


(define (gen:foobar V1 Σ V accRules [max-alts 5] [max-len 5])
  (gen:let ([raw-productions (gen:naive-ll1-ruleset V1 Σ V accRules max-alts max-len)])
           (let ([grammar-list (reverse (transform raw-productions))])
             (list raw-productions (sample (gen:grammar-derivate-data grammar-list))))))

(define (debug-print xs)
  (display-grm (car xs))
  (display (cdr xs)))

; testar se é LL(1)
; testar se a palavra é aceita
;(debug-print (last (sample (gen:foobar '(A B C) '(a b c d e) '(A B C) '() 5 5) 5)))

(define debug-production-foo
  (list
   (Production (NT 'C) (Seq (Seq (T 'b) (NT 'A)) (T 'e)))
   (Production (NT 'B) (Alt (Seq (Seq (T 'b) (T 'e)) (T 'd)) (Alt (Seq (Seq (T 'c) (NT 'B)) (T 'c)) (Seq (Seq (T 'd) (NT 'C)) (NT 'B)))))
   (Production (NT 'A) (Seq (T 'd) (NT 'C)))
   ))