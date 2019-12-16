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

#lang racket/base

(provide 
         (rename-out [_top-interaction #%top-interaction]
                     [_module-begin    #%module-begin]
                     [_datum           #%datum]
                     [_app             #%app]
                     [_top             #%top]
                     [_=               =]
                     [_inc             inc]
                     [_-               -]
                     [_<               <]
                     [_if              if]
                     [_lambda          lambda]
                     [_nil             nil]
                     [_cons            cons]
                     [_list            list]
                     [_isnil           isnil]
                     [_car             car]
                     [_cdr             cdr]
                     [_callcc          callcc]))

(require (for-syntax (only-in racket/base
                              syntax
                              raise-syntax-error
                              #%app
                              #%datum
                              quote
                              lambda)
                     (only-in syntax/parse
                              syntax-parse
                              boolean
                              str
                              exact-integer
                              id))
         
         (only-in "trif-cek.rkt"
                  ev
                  eq
                  str
                  int
                  inc
                  neg
                  lt
                  bool
                  cnd
                  fn
                  app
                  fix
                  nil
                  pair
                  isnil
                  hd
                  tl
                  call-cc
                  k?)
         (only-in racket/match
                  define/match))

(define-syntax (_let stx)
  (syntax-parse stx
    [(_ x:id e1 e2) #'(app (fn (list x) e2) (list e1))]))

(define-syntax (_letrec stx)
  (syntax-parse stx
    [(_ (f:id xi:id ...) ebody e2) #'(_let f (fix (fn (cons f (list xi ...)) ebody)) e2)]))

(define-syntax (_begin stx)
  (syntax-parse stx
    #:datum-literals (define)
    [(_ e1)                                            #'e1]
    [(_ (define x1:id e1) e2 ej ...)                   #'(_let x1 e1 (_begin e2 ej ...))]
    [(_ (define (f:id xi:id ...) e1 ei ...) e2 ej ...) #'(_letrec (f xi ...) (_begin e1 ei ...) (_begin e2 ej ...))]))

(define-syntax (_module-begin stx)
  (syntax-parse stx
    [(_ ei ...) #'(#%module-begin (displayln (pp (ev (_begin ei ...)))))]))

(define-syntax (_top-interaction stx)
  (syntax-parse stx
    [(_ . e1) #'(displayln (pp (ev e1)))]))

(define-syntax (_datum stx)
  (syntax-parse stx
    [(_ . v:boolean)       #'(bool (#%datum . v))]
    [(_ . v:str)           #'(str (#%datum . v))]
    [(_ . v:exact-integer) #'(int (#%datum . v))]
    [(_ . other)           (raise-syntax-error 'syntax
                                               "datum must be a Boolean, string, or integer"
                                               #'other)]))

(define-syntax (_app stx)
  (syntax-parse stx
    [(_ f args ...) #'(app f (list args ...))]))

(define-syntax (_top stx)
  (syntax-parse stx
    [(_ . i) #'(quote i)]))

(define-syntax (_= stx)
  (syntax-parse stx
    [(_ e1 e2) #'(eq e1 e2)]))

(define-syntax (_inc stx)
  (syntax-parse stx
    [(_ e1) #'(inc e1)]))

(define-syntax (_- stx)
  (syntax-parse stx
    [(_ e1) #'(neg e1)]))

(define-syntax (_< stx)
  (syntax-parse stx
    [(_ e1 e2) #'(lt e1 e2)]))

(define-syntax (_if stx)
  (syntax-parse stx
    [(_ e0 e1 e2) #'(cnd e0 e1 e2)]))

(define-syntax (_lambda stx)
  (syntax-parse stx
    [(_ (x:id ...) ebody) #'(fn (list x ...) ebody)]))

(define-syntax (_nil stx)
  #'(nil))

(define-syntax (_cons stx)
  (syntax-parse stx
    [(_ e1 e2) #'(pair e1 e2)]))

(define-syntax (_list stx)
  (syntax-parse stx
    [(_) #'_nil]
    [(_ e1 ei ...) #'(pair e1 (_list ei ...))]))

(define-syntax (_isnil stx)
  (syntax-parse stx
    [(_ e1) #'(isnil e1)]))

(define-syntax (_car stx)
  (syntax-parse stx
    [(_ e1) #'(hd e1)]))

(define-syntax (_cdr stx)
  (syntax-parse stx
    [(_ e1) #'(tl e1)]))

(define-syntax (_callcc stx)
  (syntax-parse stx
    [(_ e1) #'(call-cc e1)]))






;;=============================================================
;; Pretty Printing
;;=============================================================

(define/match (pp e)

  [((str s))
   (format "\"~a\"" s)]

  [((int i))
   (number->string i)]

  [((bool b))
   (format "~a" b)]

  [((fn _ _))
   "[closure]"]

  [((nil))
   "nil"]

  [((pair e1 e2))
   (string-append "(pair " (pp e1) " " (pp e2) ")")]

  [(k1) #:when (k? k1)
   "[continuation]"]

  ;; TODO: errors
  ;; TODO: general expressions
  )

