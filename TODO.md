# QOL short term todos
- [x] Handle all existing parsing errors and append to diagnostics
- [x] Group all inline errors into a type
- [x] Return from all sub-parsers and synchronize the parser by moving to the next statement after an ;
- [ ] Record errors during compilation just like it is done in the lexer and parser
- [ ] Consider a better way of handling an expression that could be returned (e.g. x + 1)
    vs a statement that returns nothing (e.g. x + 1;, followed by a semicolon)
- [ ] Change Void into Unit
- [ ] Fix wrong error pointer into the source code
- [ ] Do not store source code in the token span object, instead store indices and query the source code

# Features
- [ ] Parse function types in let statements and allow these functions
- [ ] Figure out string implementations
- [ ] Treesitter grammar to unlock syntax highlighting
- [ ] Optional returns
