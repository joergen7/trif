#lang racket/base
(require redex)

(define-language trif
  
  (x ::=
     variable-not-otherwise-mentioned)

  (s ::=
     string)

  (i ::=
     integer)

  (b ::=
     true
     false)

  (e ::=
     
     (eq e e)          ; comparison: strings, Booleans, integers, lists (deep) but no functions

     (str s)          ; strings
     
     (int i)          ; integers
     (inc e)
     (neg e)
     (lt e e)

     b
     (if e e e)

     x                ; lambda calculus
     (fn (x ...) e)
     (app e (e ...))
     (fix e)

     nil              ; lists
     (pair e e)
     (nil? e)
     (car e)
     (cdr e)

     (call-cc e)
     (cc E)
     
     error)

  (v ::=
     (str s)
     (int i)
     b
     (fn (x ...) e)
     nil
     (pair v v)
     (cc E))
  
  (E ::=
     hole
     (eq E e)
     (eq v E)
     (inc E)
     (neg E)
     (lt E e)
     (lt v E)
     (if E e e)
     (app E (e ...))
     (app v (v ... E e ...))
     (fix E)
     (pair E e)
     (pair v E)
     (nil? E)
     (car E)
     (cdr E)
     (call-cc E))


  #:binding-forms
  (fn (x ...) e #:refers-to (shadow x ...)))

(define-metafunction trif _not : e -> e
  [(_not e_1) (if e_1 false true)])

(define-metafunction trif _or : e e -> e
  [(_or e_1 e_2) (if e_1 true e_2)])

(define-metafunction trif _and : e e -> e
  [(_and e_1 e_2) (if e_1 e_2 false)])

(define-metafunction trif _geq : e e -> e
  [(_geq e_1 e_2) (_not (lt e_1 e_2))])

(define-metafunction trif _gt : e e -> e
  [(_gt e_1 e_2) (lt (neg e_1) (neg e_2))])

(define-metafunction trif _leq : e e -> e
  [(_leq e_1 e_2) (_not (_gt e_1 e_2))])

(define-metafunction trif _dec : e -> e
  [(_dec e_1) (neg (inc (neg e_1)))])

(define-metafunction trif _let : x e e -> e
  [(_let x_1 e_1 e_r) (app (fn (x_1) e_r) (e_1))])

(define-metafunction trif _letrec : (x x ...) e e -> e
  [(_letrec (x_f x_i ...) e_body e_r) (_let x_f (fix (fn (x_f x_i ...) e_body)) e_r)])

(define-metafunction trif _begin : any ... -> e
  [(_begin e_1) e_1]
  [(_begin (define x_1 e_1) any_j ...) (_let x_1 e_1 (_begin any_j ...))]
  [(_begin (define (x_f x_i ...) e_body ...) any_j ...) (_letrec (x_f x_i ...) (_begin e_body ...) (_begin any_j ...))])
  
  
(define trif-->
  (reduction-relation
   trif
   #:domain e

   ; comparison

   (~> (eq (str s_1) (str s_2))
       ,(equal? (term s_1) (term s_2))
       E-cmp-str)

   (~> (eq b_1 b_2)
       ,(equal? (term b_1) (term b_2))
       E-cmp-bool)
   
   (~> (eq nil nil) true E-cmp-nil)
   
   (~> (eq (pair e_11 e_12) (pair e_21 e_22))
       (_and (eq e_11 e_21) (eq e_12 e_22))
       E-cmp-pair)

   (~> (eq (int i_1) (int i_1))
       ,(equal? (term i_1) (term i_2))
       E-cmp-int)

   ; integers
   (~> (inc (int e_1))
       ,(add1 (term e_1)))
       
   ; condition
   (~> (if true e_1 e_2)
       e_1
       E-cnd-true)

   (~> (if false e_1 e_2)
       e_2
       E-cnd-false)

   ; function application
   (~> (app (fn () e_body) ())
       e_body
       E-app-base)

   (~> (app (fn (x_1 x_i ...) e_body) (v_1 v_j ...))
       (app (fn (x_i ...) (substitute e_body x_1 v_1)) (v_j ...))
       E-app-ind)

   ; fixpoint operator
   (~> (fix (fn (x_f x_i ...) e_body))
       (fn (x_i ...) (app (fn (x_f) e_body) ((fix (fn (x_f x_i ...) e_body)))))
       E-fix)


   ; lists
   (~> (nil? (pair v_1 v_2))
       false
       E-nil?-pair)

   (~> (nil? nil)
       true
       E-nil?-nil)

   (~> (car (pair v_1 v_2))
       v_1
       E-car)

   (~> (cdr (pair v_1 v_2))
       v_2
       E-cdr)

   ; continuations
   (~> (app (cc E_1) (v_1))
       (in-hole E_1 v_1))

   (--> (in-hole E (call-cc (fn (x_k) e_body)))
        (in-hole E (app (fn (x_k) e_body) ((cc E)))))

   ; errors
   (--> (in-hole E error) error)
   
   with

   ((--> (in-hole E p) (in-hole E q)) (~> p q))))


                      