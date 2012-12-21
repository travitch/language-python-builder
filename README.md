This package provides a "builder" layer on top of the
[language-python](http://hackage.haskell.org/package/language-python)
by Bernie Pope.  The builder (via the `Q` monad) acts like the `Q`
monad in Template Haskell to provide hygienic name generation.

To use this package, build up an AST in the `Q` monad and then use
`runQ` to convert it into an AST.  The pretty printers from the
language-python package can be used to turn this AST into a string.

This package will also aim to automatically parenthesize
sub-expressions (since parentheses must be explicitly added in
language-python).
