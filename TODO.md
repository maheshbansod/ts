
# Todo

things to do

## Application

- [ ] \[Defer/won't fix\] Destructuring operator should be part of expression i guess

### Syntax

- [x] Add var binding
- [x] Assign expression
- [x] ignore comments

### Operators

- [ ] Relational operators
- [ ] Comparison operators
- [x] dot operator
- [x] Associativity stuff ( + it's supporting in macro)

# Checker

- [x] Member access
- [x] Any type - assigning to and from any should be fine

### Error stuff

- [ ] Maybe use token.clone instead of consuming after peek for where I do next and expect "Already peeked"
- [ ] Look at unexpected token cases and see if expected token cases can work better there.
- [x] BUG - why errors of multiple unexpected token for the same token

### Macros

- [x] Make a macro to set priorities for operators automatically
- [ ] string -> tokenkind for keywords and operators

## Non-application

- [ ] use cargo-make or just or something
- [x] add README.md

## extr

- [ ] emit LLVM IR?
