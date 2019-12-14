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

(provide #%module-begin
         #%top-interaction
         define
         (rename-out [_datum        #%datum]
                     [_app          #%app]
                     [_top          #%top]))

(require (for-syntax (only-in racket/base
                              syntax
                              raise-syntax-error
                              #%app
                              #%datum
                              quote)
                     (only-in syntax/parse
                              syntax-parse
                              boolean
                              str
                              exact-integer))
         (only-in "trif-cek.rkt"
                  bool
                  str
                  int
                  app))

(define-syntax (_datum stx)
  (syntax-parse stx
    [(_ . v:boolean)       #'(bool (#%datum . v))]
    [(_ . v:str)           #'(str (#%datum . v))]
    [(_ . v:exact-integer) #'(int (#%datum . v))]
    [(_ . other)          (raise-syntax-error 'syntax
                                              "datum must be a Boolean, string, or integer"
                                              #'other)]))

(define-syntax (_top stx)
  (syntax-parse stx
    [(_ . i) #'(quote i)]))

(define-syntax (_app stx)
  (syntax-parse stx
    [(_ f args ...) #'(app f (list args ...))]))