#lang racket

(require rackcheck)
(require "glc-gen-utils.rkt")

;(define-type  GExp (Union String NT Symb Alt Seq))
;(define-type  Symb (Union String NT))
(struct Alt (l r) #:transparent)
(struct Seq (l r) #:transparent)
(struct NT  (String) #:transparent)

(struct Production (nt rhs) #:transparent)
;(define-type Grammar (Listof Production))

; Alt _ _ : 2
; Seq _ _ : 1
; NT _ , char, sym : 0

(define (pprint-gex ident p e)
     (match e
         [(Seq l r) (string-append (parens (<= p 1) (pprint-gex ident 1 l))
                                   (parens (<= p 1) (pprint-gex ident 1 r)))]
         [(Alt l r) (string-append ident
                                   (parens (<= p 2)  (pprint-gex ident 1 l))
                                   "\n"
                                   ident
                                   (parens (<= p 2) (pprint-gex ident 1 r)))]
        [(? string? e) e]
        [(? char? e)   (string e)]
        [(? symbol? e) (symbol->string e)]
   )
)

(define (parens b s)
    (cond
      [b    (string-append "(" s ")")]
      [else s]))

#;(define (pprin-grm xs)
    (match xs
      [(cond (Production s e) zs) (string-append s "-->" (pprint-gex (mk-ident (+ (length s) 3)) p e))])

  )

(define (mk-ident n)
    (build-string n (lambda (n) #\ )) 
  )

(define (shalow-first seq)
    (match seq
      [(Seq e d) (shalow-first e)]
      [(NT s)    (error "Shalow first: the input RHS must be in Greibach Normal Form")]
      [(Alt e d) (error "Shalow first: the input must not contain alternatives")]
      [x         x]
      )
  )

(define (gen:naive-ll1-seq Σ V [max-len 5])
        (gen:let ([s (gen:one-of Σ)]
                  [l (gen:list (gen:one-of (append Σ V)) #:max-length max-len )])
                (gen:const (foldr (lambda (x r) (Seq r x)) s l)))
  )

(define (gen:naive-ll1-alt Σ V [max-alts 5] [max-len 5])
      (cond
         [(<= (length Σ) 0) (error "gen:naive-ll1-alt can not produce any viable terms with empty alphabet")]
         [(eq? (length Σ) 1) (gen:naive-ll1-seq Σ V max-len)]
         [else (gen:let ([t (gen:naive-ll1-seq Σ V max-len)]
                         [p (gen:integer-in 0 100)])
                       (cond
                           [(or (> p 80) (<= max-alts 0)) (gen:const t)]
                           [else (gen:bind (gen:naive-ll1-alt (remove (shalow-first t) Σ)
                                                              V
                                                              (- max-alts 1)
                                                              max-len)
                                           (lambda (ts) (gen:const (Alt t ts)))) ] ))]
      )
)



(define (gen:naive-ll1-ruleset V1 Σ V accRules [max-alts 5] [max-len 5])
        (cond
          [(null? V1) (gen:const accRules)]
          [else       (gen:let ([rhs (gen:naive-ll1-alt Σ V max-alts max-len)])
                               (gen:naive-ll1-ruleset (cdr V1)
                                                      Σ
                                                      V
                                                      (cons (Production (car V1) rhs) accRules)
                                                      max-alts
                                                      max-len))]
       )
  )
