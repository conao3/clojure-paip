# clojure-paip

A Clojure implementation of examples from Peter Norvig's classic book "Paradigms of Artificial Intelligence Programming" (PAIP).

## Overview

This project ports selected AI programming techniques from PAIP to Clojure. The original book uses Common Lisp to teach fundamental AI concepts through practical implementations. This repository reimagines those examples using idiomatic Clojure.

### Implemented Chapters

**Chapter 6: Pattern Matching and ELIZA**
- Pattern matching with segment variables
- The ELIZA chatbot - a classic natural language processing demonstration

**Chapter 11: Logic Programming**
- Unification algorithm
- A working Prolog interpreter in Clojure

## Requirements

- Clojure 1.12.0 or later
- Clojure CLI tools

## Getting Started

Clone the repository and start a REPL:

```bash
clj -A:dev
```

### Running ELIZA

```clojure
(require '[cljpaip.chapter6.eliza :as eliza])
(eliza/eliza)
;; > (hello there)
;; (How do you do. Please state your problem.)
;; > (I want to test this program)
;; (Suppose you got to test this problem soon)
```

### Using the Prolog Interpreter

```clojure
(require '[cljpaip.chapter11.prolog :refer [<- ?- clear-db]])

;; Define facts and rules
(<- (likes Kim Robin))
(<- (likes Sandy Lee))
(<- (likes Sandy Kim))
(<- (likes Robin cats))
(<- (likes ?x ?x))

;; Query the database
(?- (likes Sandy ?who))
;; ?who = Lee
;; ?who = Kim
;; ?who = Sandy
```

## Running Tests

```bash
clj -M:test
```

## References

- [Paradigms of Artificial Intelligence Programming](https://github.com/norvig/paip-lisp) by Peter Norvig
- Original source code available on GitHub

## License

This project is provided for educational purposes.
