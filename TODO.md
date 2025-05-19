# QOL short term todos
- [x] Handle all existing parsing errors and append to diagnostics
- [x] Group all inline errors into a type
- [ ] Return from all sub-parsers and synchronize the parser by moving to the next statement after an ;
- [ ] Record errors during compilation just like it is done in the lexer and parser
- [ ] Consider a better way of handling an expression that could be returned (e.g. x + 1)
    vs a statement that returns nothing (e.g. x + 1;, followed by a semicolon)
