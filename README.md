# Trif

Trif is a parallel, dynamically typed functional programming language with a Lisp syntax.

## Parallelism and Determinism

Trif is a proof-of-concept for a functional programming language that is not based on an operational semantics but on a Petri net semantics. The definition naturally allows using subterm independence for parallelism.

As a consequence, Trif's evaluation model does not distinguish call-by-name and call-by-value. Lists are evaluated only when accessed.

I want the result of a Trif program to be deterministic. For that to work, there can be no side effects because that would allow independent subterms to influence one another and break determinism.

Although this approach allows using all CPUs in a computer and to have several computers cooperate I do not expect Trif to beat C at quicksort.

## Dynamic Typing

Trif is dynamically typed. Typed variants of Trif might compile to Trif as a kernel language. Providing no static type system allows several such variants to exist.

## Functional Programming

Functions are values. Lists, logic, general recursion, quote, eval. No references (mutable variables) nor mutable data type.

## Syntax

Looks like a Lisp.

