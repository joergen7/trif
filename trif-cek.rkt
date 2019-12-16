;; trif: dynamically typed functional programming language with a
;; Lisp syntax
;;
;; Copyright 2019 Jörgen Brandt <joergen@cuneiform-lang.org>
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;
;; -------------------------------------------------------------------

#lang typed/racket/base

(provide (struct-out uncomparable)
         (struct-out inc-no-int)
         (struct-out neg-no-int)
         (struct-out lt-lhs-no-int)
         (struct-out lt-rhs-no-int)
         (struct-out cnd-if-no-bool)
         (struct-out var-unbound)
         (struct-out app-no-fn)
         (struct-out app-narg)
         (struct-out fix-fn-no-arg)
         (struct-out fix-no-fn)
         (struct-out isnil-no-list)
         (struct-out hd-nil)
         (struct-out hd-no-list)
         (struct-out tl-nil)
         (struct-out tl-no-list)
         (struct-out call-cc-narg)
         (struct-out call-cc-no-fn)
         (struct-out continue-narg)
         Reason
         reason?
         
         (struct-out eq)
         (struct-out str)
         (struct-out int)
         (struct-out inc)
         (struct-out neg)
         (struct-out lt)
         (struct-out bool)
         (struct-out cnd)
         (struct-out fn)
         (struct-out app)
         (struct-out fix)
         (struct-out nil)
         (struct-out pair)
         (struct-out isnil)
         (struct-out hd)
         (struct-out tl)
         (struct-out call-cc)
         (struct-out err)
         Expr
         expr?
         Value
         value?
         Ev-Result
         ev-result?

         (struct-out eq-lhs)
         (struct-out eq-rhs)
         (struct-out inc-op)
         (struct-out neg-op)
         (struct-out lt-lhs)
         (struct-out lt-rhs)
         (struct-out cnd-if)
         (struct-out app-fn)
         (struct-out app-arg)
         (struct-out fix-op)
         (struct-out pair-lhs)
         (struct-out pair-rhs)
         (struct-out isnil-op)
         (struct-out hd-op)
         (struct-out tl-op)
         (struct-out call-cc-op)
         K
         k?

         Closure
         closure?
         Env
         env?
         (struct-out prog)

         ev
         step)

(require (only-in racket/match
                  define/match
                  match))


(struct uncomparable   ([lhs  : Value]
                        [rhs  : Value])           #:transparent)
(struct inc-no-int     ([op   : Value])           #:transparent)
(struct neg-no-int     ([op   : Value])           #:transparent)
(struct lt-lhs-no-int  ([lhs  : Value])           #:transparent)
(struct lt-rhs-no-int  ([rhs  : Value])           #:transparent)
(struct cnd-if-no-bool ([if   : Value])           #:transparent)
(struct var-unbound    ([x    : Symbol])          #:transparent)
(struct app-no-fn      ([fn   : Value])           #:transparent)
(struct app-narg       ([xs   : (Listof Symbol)]
                        [args : (Listof Value)])  #:transparent)
(struct fix-fn-no-arg  ([op   : Value])           #:transparent)
(struct fix-no-fn      ([op   : Value])           #:transparent)
(struct isnil-no-list  ([op   : Value])           #:transparent)
(struct hd-nil         ()                         #:transparent)
(struct hd-no-list     ([op   : Value])           #:transparent)
(struct tl-nil         ()                         #:transparent)
(struct tl-no-list     ([op   : Value])           #:transparent)
(struct call-cc-narg   ([xs   : (Listof Symbol)]) #:transparent)
(struct call-cc-no-fn  ([op   : Value])           #:transparent)
(struct continue-narg  ([args : (Listof Expr)])   #:transparent)
  

(define-type Reason
  (U uncomparable
     inc-no-int
     neg-no-int
     lt-lhs-no-int
     lt-rhs-no-int
     cnd-if-no-bool
     var-unbound
     app-no-fn
     app-narg
     fix-fn-no-arg
     fix-no-fn
     isnil-no-list
     hd-nil
     hd-no-list
     tl-nil
     tl-no-list
     call-cc-narg
     call-cc-no-fn
     continue-narg))

(define-predicate reason? Reason)


(struct eq      ([e1     : Expr]
                 [e2     : Expr])          #:transparent)
(struct str     ([s      : String])        #:transparent)
(struct int     ([i      : Integer])       #:transparent)
(struct inc     ([e      : Expr])          #:transparent)
(struct neg     ([e      : Expr])          #:transparent)
(struct lt      ([e1     : Expr]
                 [e2     : Expr])          #:transparent)
(struct bool    ([b      : Boolean])       #:transparent)
(struct cnd     ([e0     : Expr]
                 [e1     : Expr]
                 [e2     : Expr])          #:transparent)
(struct fn      ([xs     : (Listof Symbol)]
                 [body   : Expr])          #:transparent)
(struct app     ([e0     : Expr]
                 [args   : (Listof Expr)]) #:transparent)
(struct fix     ([e      : Expr])          #:transparent)
(struct nil     ()                         #:transparent)
(struct pair    ([e1     : Expr]
                 [e2     : Expr])          #:transparent)
(struct isnil   ([e      : Expr])          #:transparent)
(struct hd      ([e      : Expr])          #:transparent)
(struct tl      ([e      : Expr])          #:transparent)
(struct call-cc ([e      : Expr])          #:transparent)
(struct err     ([reason : Reason])        #:transparent)

(define-type Expr
  (U eq
     str
     int
     inc
     neg
     lt
     bool
     cnd
     Symbol
     fn
     app
     fix
     nil
     pair
     isnil
     hd
     tl
     call-cc
     K
     err))

(define-predicate expr? Expr)

(define-type Value
  (U str
     int
     bool
     fn
     nil
     pair
     K))

(define-predicate value? Value)

(define-type Ev-Result
  (U Value
     err))

(define-predicate ev-result? Ev-Result)


(struct mt         ()                #:transparent)
(struct eq-lhs     ([rhs       : Expr]
                    [env       : Env]
                    [k         : K]) #:transparent)
(struct eq-rhs     ([lhs       : Value]
                    [k         : K]) #:transparent)
(struct inc-op     ([k         : K]) #:transparent)
(struct neg-op     ([k         : K]) #:transparent)
(struct lt-lhs     ([rhs       : Expr]
                    [env       : Env]
                    [k         : K]) #:transparent)
(struct lt-rhs     ([lhs       : Value]
                    [k         : K]) #:transparent)
(struct cnd-if     ([e1        : Expr]
                    [e2        : Expr]
                    [env       : Env]
                    [k         : K]) #:transparent)
(struct app-fn     ([args      : (Listof Expr)]
                    [env-app   : Env]
                    [k         : K]) #:transparent)
(struct app-arg    ([fn        : Value]
                    [env-fn    : Env]
                    [fin-args  : (Listof Value)]
                    [todo-args : (Listof Expr)]
                    [env-app   : Env]
                    [k         : K]) #:transparent)
(struct fix-op     ([env       : Env]
                    [k         : K]) #:transparent)
(struct pair-lhs   ([rhs      : Expr]
                    [env      : Env]
                    [k         : K]) #:transparent)
(struct pair-rhs   ([lhs      : Value]
                    [env      : Env]
                    [k         : K]) #:transparent)
(struct isnil-op   ([k         : K]) #:transparent)
(struct hd-op      ([env      : Env]
                    [k         : K]) #:transparent)
(struct tl-op      ([env      : Env]
                    [k         : K]) #:transparent)
(struct call-cc-op ([k         : K]) #:transparent)

(define-type K
  (U mt
     eq-lhs
     eq-rhs
     inc-op
     neg-op
     lt-lhs
     lt-rhs
     cnd-if
     app-fn
     app-arg
     fix-op
     pair-lhs
     pair-rhs
     isnil-op
     hd-op
     tl-op
     call-cc-op))

(define-predicate k? K)


(define-type Closure (Pairof Value Env))
(define-predicate closure? Closure)

(define-type Env (Listof (Pairof Symbol Closure)))
(define-predicate env? Env)

(struct prog ([cs        : Expr]
              [env       : Env]
              [k         : K]
              [cs-value? : Boolean]) #:transparent)


;;=============================================================
;; Evaluation
;;=============================================================

(: ev (Expr -> Ev-Result))
(define (ev e)

  (: ev* (prog -> prog))
  (define (ev* p0)

    (define p1 : (U prog False)
      (step p0))

    (if p1
        (ev* p1)
        p0))

  (define p0 : prog
    (prog e '() (mt) #t))

  (match (ev* p0)
    [(prog e1 _ (mt) _) (assert e1 ev-result?)]))
    


(: step (prog -> (U prog False)))
(define/match (step p)

  ;-----------------------------
  ; comparison
  ;-----------------------------

  [((prog (eq e1 e2) env k _))
   (prog e1 env (eq-lhs e2 env k) #f)]

  [((prog e1 _ (eq-lhs e2 env k) #t))
   (prog e2 env (eq-rhs (assert e1 value?) k) #f)]

  [((prog (str s2) _ (eq-rhs (str s1) k) _))              ; E-cmp-str
   (if (equal? s1 s2)
       (prog (bool #t) '() k #t)
       (prog (bool #f) '() k #t))]
   
  [((prog (int i2) _ (eq-rhs (int i1) k) _))              ; E-cmp-int
   (if (equal? i1 i2)
       (prog (bool #t) '() k #t)
       (prog (bool #f) '() k #t))]

  [((prog (bool a) _ (eq-rhs (bool b) k) _))              ; E-cmp-bool
   (if (equal? a b)
       (prog (bool #t) '() k #t)
       (prog (bool #f) '() k #t))]

  [((prog (nil) _ (eq-rhs (nil) k) _))                    ; E-cmp-nil-nil
   (prog (bool #t) '() k #t)]

  [((prog (pair _ _) _ (eq-rhs (nil) k) _))               ; E-cmp-nil-cons
   (prog (bool #f) '() k #t)]

  [((prog (nil) _ (eq-rhs (pair _ _) k) _))               ; E-cmp-cons-nil
   (prog (bool #f) '() k #t)]

  [((prog (pair e21 e22) _ (eq-rhs (pair e11 e12) k) #t)) ; E-cmp-cons-cons
   (prog (_and (eq e11 e21) (eq e12 e22)) '() k #f)]

  [((prog e2 _ (eq-rhs e1 k) #t))
   (prog (err (uncomparable e1 (assert e2 value?))) '() (mt) #f)]

  ;-----------------------------
  ; strings
  ;-----------------------------

  [((prog (str s1) _ k #f))          ; strings deflect
   (prog (str s1) '() k #t)]

  ;-----------------------------
  ; integers
  ;-----------------------------

  [((prog (int i1) _ k #f))          ; integers deflect
   (prog (int i1) '() k #t)]

  [((prog (inc e1) env k _))
   (prog e1 env (inc-op k) #f)]

  [((prog (int i) _ (inc-op k) _))
   (prog (int (add1 i)) '() k #t)]

  [((prog e1 _ (inc-op k) #t))
   (prog (err (inc-no-int (assert e1 value?))) '() (mt) #f)]

  [((prog (neg e1) env k _))
   (prog e1 env (neg-op k) #f)]

  [((prog (int i) _ (neg-op k) _))
   (prog (int (- i)) '() k #t)]

  [((prog e1 _ (neg-op k) #t))
   (prog (err (neg-no-int (assert e1 value?))) '() (mt) #f)]
  
  [((prog (lt e1 e2) env k _))
   (prog e1 env (lt-lhs e2 env k) #f)]

  [((prog e1 _ (lt-lhs e2 env k) #t))
   (prog e2 env (lt-rhs (assert e1 value?) k) #f)]

  [((prog (int i2) _ (lt-rhs (int i1) k) _))
   (prog (bool (< i1 i2)) '() k #t)]

  [((prog (int _) _ (lt-rhs v1 k) _))
   (prog (err (lt-lhs-no-int v1)) '() (mt) #f)]

  [((prog e2 _ (lt-rhs (int _) k) #t))
   (prog (err (lt-rhs-no-int (assert e2 value?))) '() (mt) #f)]

  ;-----------------------------
  ; Booleans
  ;-----------------------------

  [((prog (bool b) _ k #f))          ; Booleans deflects
   (prog (bool b) '() k #t)]

  [((prog (cnd e0 e1 e2) env k _))
   (prog e0 env (cnd-if e1 e2 env k) #f)]

  [((prog (bool #t) _ (cnd-if e1 _ env k) _))
   (prog e1 env k #f)]

  [((prog (bool #f) _ (cnd-if _ e2 env k) _))
   (prog e2 env k #f)]

  [((prog e0 _ (cnd-if _ _ _ k) #t))
   (prog (err (cnd-if-no-bool (assert e0 value?))) '() (mt) #f)]

  ;-----------------------------
  ; lambda calculus
  ;-----------------------------

  [((prog x env k _)) #:when (symbol? x)                                       ; variable look up
   (match (assoc x env)
     [(cons _ (cons e1 env1)) (prog e1 env1 k #f)]
     [#f                      (prog (err (var-unbound x)) '() (mt) #f)])]

  [((prog (fn xs e-body) env k #f))                                            ; functions deflect
    (prog (fn xs e-body) env k #t)]
  
  [((prog (app e0 args) env k _))                                              ; descend application's
   (prog e0 env (app-fn args env k) #f)]                                       ; function position

  [((prog (fn '() e-body) env-fn (app-fn '() _ k) _))                          ; use body if function has
   (prog e-body env-fn k #f)]                                                  ; no argument

  [((prog (fn xs e-body) env-fn (app-fn (cons arg1 args) env-app k) _))        ; switch to arguments
   (prog arg1 env-app (app-arg (fn xs e-body) env-fn '() args env-app k) #f)]  ; if function has any

  [((prog k1 _ (app-fn (list e21) env-app _) _)) #:when (k? k1)                ; continue with expression
   (prog e21 env-app k1 #f)]

  [((prog k1 env-fn (app-fn args _ _) _)) #:when (k? k1)                       ; error if continue does not
   (prog (err (continue-narg args)) '() (mt) #f)]                              ; have exactly one argument
  
  [((prog e-fn _ (app-fn _ _ k) #t))                                           ; error if neither function
   (prog (err (app-no-fn (assert e-fn value?))) '() (mt) #f)]                  ; nor continuation

  [((prog e1 _ (app-arg v-fn env-fn fin-args (cons arg1 args) env-arg k) #t))  ; switch to next argument
   (prog arg1 env-arg (app-arg v-fn env-fn (cons (assert e1 value?) fin-args) args env-arg k) #f)]

  [((prog e1 _ (app-arg (fn xs e-body) env-fn fin-args '() env-arg k) #t))     ; application extends
   (let ([args : (Listof Value) (reverse (cons (assert e1 value?) fin-args))]) ; closure
     (if (equal? (length xs) (length args))
         (let ([env1 : Env (for/fold ([env-acc : Env    env-fn])
                                     ([x       : Symbol (in-list xs)]
                                      [arg     : Value  (in-list args)])
             
                             (define c : Closure
                               (cons arg env-arg))

                             (define pair : (Pairof Symbol Closure)
                               (cons x c))
         
                             (cons pair env-acc))])

           (prog e-body env1 k #f))
         (prog (err (app-narg xs args)) '() (mt) #f)))]

  [((prog (fix e1) env k _))                                                   ; descend fixpoint
   (prog e1 env (fix-op env k) #f)]

  [((prog (fn (cons x-f xs) e-body) _ (fix-op env k) _))                       ; fixpoint replicates
   (prog (fn xs (_let x-f (fix (fn (cons x-f xs) e-body)) e-body)) env k #t)]

  [((prog (fn '() e-body) _ (fix-op _ k) _))                                   ; error if fixpoint operand
   (prog (err (fix-fn-no-arg (fn '() e-body))) '() (mt) #f)]                   ; has no argument

  [((prog e1 _ (fix-op _ k) #t))                                               ; error if fixpoint operand
   (prog (err (fix-no-fn (assert e1 value?))) '() (mt) #f)]                    ; is no function

  
  ;-----------------------------
  ; lists
  ;-----------------------------

  [((prog (nil) _ k #f))             ; nil deflects
   (prog (nil) '() k #t)]

  [((prog (pair e1 e2) env k #f))
   (prog e1 env (pair-lhs e2 env k) #f)]

  [((prog e1 _ (pair-lhs e2 env k) #t))
   (prog e2 env (pair-rhs (assert e1 value?) env k) #f)]

  [((prog e2 _ (pair-rhs v1 env k) #t))
   (prog (pair v1 e2) env k #t)]

  [((prog (isnil e1) env k _))
   (prog e1 env (isnil-op k) #f)]

  [((prog (nil) _ (isnil-op k) _))
   (prog (bool #t) '() k #t)]

  [((prog (cons _ _) _ (isnil-op k) _))
   (prog (bool #f) '() k #t)]

  [((prog e1 _ (isnil-op k) #t))
   (prog (err (isnil-no-list (assert e1 value?))) '() (mt) #f)]

  [((prog (hd e1) env k _))
   (prog e1 env (hd-op env k) #f)]

  [((prog (pair e11 _) _ (hd-op env k) #t))
   (prog e11 env k #t)]

  [((prog (nil) _ (hd-op _ k) _))
   (prog (err (hd-nil)) '() (mt) #f)]

  [((prog e1 _ (hd-op _ k) #t))
   (prog (err (hd-no-list (assert e1 value?))) '() (mt) #f)]

  [((prog (tl e1) env k _))
   (prog e1 env (tl-op env k) #f)]

  [((prog (cons _ e12) _ (tl-op env k) #t))
   (prog e12 env k #t)]
  
  [((prog (nil) _ (tl-op _ k) _))
   (prog (err (tl-nil)) '() (mt) #f)]

  [((prog e1 _ (tl-op _ k) #t))
   (prog (err (tl-no-list (assert e1 value?))) '() (mt) #f)]

  ;-----------------------------
  ; continuations
  ;-----------------------------

  [((prog k1 _ k #f)) #:when (k? k1)                            ; continuations deflect
   (prog k1 '() k #t)]

  [((prog (call-cc e1) env k _))                                ; descend call-cc operand
   (prog e1 env (call-cc-op k) #f)]

  [((prog (fn (list x-k) e-body) env (call-cc-op k) _))         ; call with current continuation
   (prog (_let x-k k e-body) env k #f)]                         ; if call-cc operand is a function

  [((prog (fn xs e-body) _ (call-cc-op k) _))                   ; error if call-cc operand function
   (prog (err (call-cc-narg xs)) '() (mt) #f)]                  ; does not have exactly one argument

  [((prog e1 _ (call-cc-op k) #t))                              ; error if call-cc operand is no
   (prog (err (call-cc-no-fn (assert e1 value?))) '() (mt) #f)] ; function

  
  ;-----------------------------
  ; error
  ;-----------------------------

  [((prog (err r) _ k _)) #:when (pair? k) ; E-error
   (prog (err r) '() '() #f)]
  

  ;-----------------------------
  ; no progress possible
  ;-----------------------------

  [(_)
   #f])



;;=============================================================
;; Internal Functions
;;=============================================================

(: _and (Expr Expr -> Expr))
(define (_and e1 e2)
  (cnd e1 e2 (bool #f)))

(: _let (Symbol Expr Expr -> Expr))
(define (_let x e1 e2)
  (app (fn (list x) e2) (list e1)))


       
;;=============================================================
;; Unit Tests
;;=============================================================

(module+ test

  (require typed/rackunit)

  (check-equal? (ev (eq (str "a") (str "b")))
                (bool #f)
                "compare unequal strings produces false")

  (check-equal? (ev (eq (str "a") (str "a")))
                (bool #t)
                "compare equal strings produces true")

  (check-equal? (ev (eq (int 1) (int 2)))
                (bool #f)
                "compare unequal integers produces false")

  (check-equal? (ev (eq (int 1) (int 1)))
                (bool #t)
                "compare equal integers produces true")

  (check-equal? (ev (eq (int 1) (str "1")))
                (err (uncomparable (int 1) (str "1")))
                "compare integer and string produces error")

  (check-equal? (ev (eq (fn '(x) 'x) (fn '(x) 'x)))
                (err (uncomparable (fn '(x) 'x) (fn '(x) 'x)))
                "compare functions produces error")

  )