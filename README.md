# Asra
The second version of the Asra programming language (here for archival purposes and to document the process of  learning to build a compiler).

My second attempt at writing a compiler for a functional programming language (first one can be found [here](https://github.com/SpacialCircumstances/AsraLang)).

## Comparison to first version

This second version has a somewhat better parser, but it still has several bugs (especially related to comments and newlines). I tried to rewrite it into a separate tokenization step before parsing which would have helped with these issues, but never finished (mostly due to the fact that FParsec only supports parsing character streams). 

This version has a working, correct typechecker for HM! 
The first one was written based on [http://okmij.org/ftp/ML/generalization.html](http://okmij.org/ftp/ML/generalization.html). I then tried to rewrite it into a constraint-based typechecker to make implementing more features easier and adapted [https://github.com/kseo/poly_constraints](https://github.com/kseo/poly_constraints) from Haskell. Sadly, because the original code was written in Haskell and was heavily dependent on lazyness, this version is a lot less efficient, but more readable.

This version also has a transformation pipeline AST -> IR -> Typed IR to make typechecking easier. The IR is a simplified version of the AST and somewhat comparable to an extended simply typed lambda calculus.

In comparison to the first Asra version, this one has no compiler backend- it is not possible to run or compile Asra code in any way.
