# Trif

Trif is a parallel, dynamically typed functional programming language with a Lisp syntax.

## Dynamic Typing

Trif is only dynamically typed so that typed variants of the language compile to Trif as a kernel language. The independence of Trif from a concrete type system allows several such variants to exist.

## Parallelism and Determinism

The result of a Trif program is guaranteed to be deterministic. This means there can never be side effects in the language because that would allow independent subterms to influence one another and break determinism.

Trif's evaluation model does not distinguish call-by-name and call-by-value. Lists are evaluated only when accessed.

## Functional Programming

Functions are values. Lists, logic, general recursion, quote, eval.

## Syntax

Looks like a Lisp.

