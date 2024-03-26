## Code generation

This package contains low-level code generators.  The semantic graph is represented
using the expression language in the `.exl` package organized into constituant parts
using the `DomainCode` class.  The graph is language-independent, but there is
a path for low-level code generators to do transformations to fix problematic
issues.  For example, the `C` code generator does some rewriting to transform
&lambda; expressions into something more C friendly (a function and a pointer to
the function.

Otherwise, the low-level code generators are really just pretty-printers.
