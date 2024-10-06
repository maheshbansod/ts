

## responsibiltiies of the checker

1. report type information which can be used to emit .d.ts files maybe
2. report all the type errors

## inputs

- parse tree
- (future) maybe existing type information?
- tsconfig.json?

## errors

Errors need to be as informative as or more informative than the @micorosoft/typescript

- Location of all the involved tokens

let's say we have `a + b` and b is string and a is number. Then the error should be like 
```
[<error code>] <file-name>: <row>:<col>: b expected to be a number since a is a number and + here expects its rhs to be a number.
```
does this work?

the error has a and b and + basically the whole (sub)expression i guess + a reason enum,

can i say a type is always associated with an expression? what about a statement?

## checker

i suppose the checker should not consume the parse tree, but rather just go through it, and collect all errors and type information it needs to to finally emit type information and errors and cloning any tokens it needs along the way.

i think this is the way.

## whats in a type

what is type information really?

basically i need to associate an identifier / symbol / variable, idk the terminology.. basically anything that holds something, with a type.

so how about holding a reference? idk i don't really want to overcompllicate it.. but i also don't want to implement copy on an atom yet/ever.

hm, let's go with something super minimal and hold references for now.

## optimisations

(low priority)

identifiers and positions etc don't really matter.. i think i can heavily use cache for optimisation, since interaction of same types with same operators would yield the same resultant type.
