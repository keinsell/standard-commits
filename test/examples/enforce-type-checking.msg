add!(lib/type-check)[rel]: enforce type checking in function calls

Previously, the semantic analyzer allowed mismatched parameter types
in function calls, leading to runtime errors. This fix implements
strict type validation during the semantic analysis phase.

Breaking: The `validateCall` function now returns `TypeMismatchError`
  instead of returning a boolean, requiring updates in error handling.
Fixes: #247
Co-authored-by: Foo Bar <foo.bar@compiler.dev>
