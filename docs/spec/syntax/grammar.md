# Formal Grammar

This is the formal grammar for the _syntax_ of the Conical programming language.
It uses an extended Backus-Naur form (EBNF) with the addition of negation (`~ expression`) and unicode values (`U+XXXXXX`)

## Incomplete/Todo

- [ ] Pattern Matching
- [ ] Macro declarations
- [ ] Attribute-like macros / Function like macros
- [ ] Builtins / Intrinsics

## Comments

Comments are ignored during lexing and as such do not contribute to the AST.
There are two types of comments:

1. A line comment, which starts with `//` and skips all characters until a line end character is encountered (either U+00000A or U+00000A U+00000D)
2. A block comment starts with `/*` and ends with `*/`, any text in between is ignored including newlines. They can also be arbitrarily nested.

There is, in addition, another type of ignored text which is the unix "shebang" line. This is treated the same as a line comment and starts with `#!`.

## Identifiers, Keywords, and Digits

```ebnf
IDENTIFIER       ::= [A-Za-z_] [A-Za-z0-9_]*
BINARY_DIGIT     ::= [01]
DIGIT            ::= [0-9]
DIGIT_ONE        ::= [1-9]
OCTAL_DIGIT      ::= [0-7]
HEX_DIGIT        ::= [0-9a-fA-F]
KEYWORD_IF       ::= 'if'
KEYWORD_ELSE     ::= 'else'
KEYWORD_WHILE    ::= 'while'
KEYWORD_FOR      ::= 'for'
KEYWORD_IN       ::= 'in'
KEYWORD_MATCH    ::= 'match'
KEYWORD_FN       ::= 'fn'
KEYWORD_INLINE   ::= 'inline'
KEYWORD_PUB      ::= 'pub'
KEYWORD_EXPORT   ::= 'export'
KEYWORD_IMPORT   ::= 'import'
KEYWORD_LET      ::= 'let'
KEYWORD_MUT      ::= 'mut'
KEYWORD_RETURN   ::= 'return'
KEYWORD_BREAK    ::= 'break'
KEYWORD_STRUCT   ::= 'struct'
KEYWORD_ENUM     ::= 'enum'
KEYWORD_USE      ::= 'use'
KEYWORD_MOD      ::= 'mod'
KEYWORD_COMPTIME ::= 'comptime'
KEYWORD_CONTINUE ::= 'continue'
KEYWORD_AS       ::= 'as'
KEYWORD_STATIC   ::= 'static'
KEYWORD_TYPE     ::= 'type'
KEYWORD_CONST    ::= 'const'
KEYWORD_IMPL     ::= 'impl'
KEYWORD_WHEN     ::= 'when'
KEYWORD_SELF     ::= 'Self'
KEYWORD_WHERE    ::= 'where'
KEYWORD_WITH     ::= 'with'
KEYWORD_MACRO    ::= 'macro'
KEYWORD_DO       ::= 'do'
KEYWORD_PURE     ::= 'pure'
KEYWORD_TRUE     ::= 'true'
KEYWORD_FALSE    ::= 'false'
```

## Top-Level

```ebnf
PROGRAM ::= MODULE_DECLARATION DECLARATION*
```


## Escape Sequences

These are the valid escape sequences for strings and characters:
```ebnf
ESCAPE_SEQUENCE        ::= '\\' ([0ntrvfb'"\] 
                         | 'x' {HEX_DIGIT, 2}
                         | 'u' {HEX_DIGIT, 4}
                         | 'U' {HEX_DIGIT, 8}

UNICODE_NON_CHARACTERS ::= [U+00FDD0-U+00FDEF] 
                         | [U+00FFFE-U+00FFFF]
                         | [U+01FFFE-U+01FFFF]
                         | [U+02FFFE-U+02FFFF]
                         | [U+03FFFE-U+03FFFF]
                         | [U+04FFFE-U+04FFFF]
                         | [U+05FFFE-U+05FFFF]
                         | [U+06FFFE-U+06FFFF]
                         | [U+07FFFE-U+07FFFF]
                         | [U+08FFFE-U+08FFFF]
                         | [U+09FFFE-U+09FFFF]
                         | [U+0AFFFE-U+0AFFFF]
                         | [U+0BFFFE-U+0BFFFF]
                         | [U+0CFFFE-U+0CFFFF]
                         | [U+0DFFFE-U+0DFFFF]
                         | [U+0EFFFE-U+0EFFFF]
                         | [U+0FFFFE-U+0FFFFF]
                         | [U+10FFFE-U+10FFFF]

```

## Literals

```ebnf
DISALLOWED_CHARACTERS ::= [U+000000-U+00001F]
                        | [U+00007F]
                        | [U+000080-U+00009F] 
                        | [U+00D800-U+00DFFF] 
                        | UNICODE_NON_CHARACTERS

BOOL_LITERAL          ::= KEYWORD_TRUE | KEYWORD_FALSE
CHAR_LITERAL          ::= '\'' (~ DISALLOWED_CHARACTERS) | ESCAPE_SEQUENCE '\''
STRING_LITERAL        ::= '"' ( (~ DISALLOWED_CHARACTERS) | ESCAPE_SEQUENCE )* '"'
RAW_STRING_LITERAL    ::= '`' (~ DISALLOWED_CHARACTERS)* | [U+00000A] | [U+00000D] '`'

SIGN                  ::= [+-]
INTEGER_LITERAL       ::= {SIGN} DECIMAL_LITERAL | BINARY_LITERAL | OCTAL_LITERAL | HEX_LITERAL
DECIMAL_LITERAL       ::= (DIGIT_ONE | '_') (DIGIT | '_')*
BINARY_LITERAL        ::= '0' [bB] (BINARY_DIGIT | '_')+
OCTAL_LITERAL         ::= '0' [oO] (OCTAL_DIGIT | '_')+
HEX_LITERAL           ::= '0' [xX] (HEX_DIGIT | '_')+

FLOAT_LITERAL         ::= {SIGN} DECIMAL_LITERAL ( '.' DECIMAL_LITERAL | '.' DECIMAL_LITERAL [eE] {SIGN} DIGIT*)

RANGE_LITERAL         ::= (INTEGER_LITERAL | FLOAT_LITERAL) {'!'} '..' {'!'} (INTEGER_LITERAL | FLOAT_LITERAL)

SYMBOL                ::= '.' IDENTIFIER
```


## Operators 

### Arithmetic Operators

This is the precedence table for arithmetic operators:

| Precedence (High -> Low) | Operator(s)                   | Description                                         | Example                      |
| -------------------------|-------------------------------|-----------------------------------------------------|------------------------------|
| 1                        | `()`, `{}`                    | Grouping                                            | `(a+b)*c`, `{ let a = 1; a}` |
| 2                        | `.`, `\|>`                    | Dot Operator / Access Operator, Sequence Operator   | `a.b`, `1 \|> add(2)`         |
| 3                        | `expr(params)`                | Function Call                                       | `add(1, 2)`                  |
| 4                        | `!`, `~`, `+`, `-`            | Logical NOT, Bitwise NOT, Unary plus/minus          | `!x`, `~x`, `-x`             |
| 5                        | `*`, `/`, `%`                 | Multiply, Divide, Modulus                           | `a*b`, `a/b`, `a%b`          |
| 6                        | `+`, `-`                      | Add, Subtract                                       | `a+b`, `a-b`                 |
| 7                        | `<<`, `>>`                    | Bit shifts                                          | `x<<1`, `x>>1`               |
| 8                        | `<`, `<=`, `>`, `>=`          | Relational comparison                               | `a < b`                      |
| 9                        | `==`, `!=`                    | Equality                                            | `a == b`                     |
| 10                       | `&`                           | Bitwise AND                                         | `a & b`                      |
| 11                       | `^`                           | Bitwise XOR                                         | `a ^ b`                      |
| 12                       | `\|`                          | Bitwise OR                                          | `a \| b`                     |
| 13                       | `&&`                          | Logical AND                                         | `a && b`                     |
| 14                       | `\|\|`                        | Logical OR                                          | `a \|\| b`                   |




### Type Operators

This is the precedence table for type operators:

| Precedence (High -> Low) | Operator(s)                   | Description                                          | Example                     |
|--------------------------|-------------------------------|------------------------------------------------------|-----------------------------|
| 1                        | `()`                          | Grouping                                             | `(A \| B) * C`               |
| 2                        | `id: expr`                    | Label                                                | `x: i32`                    |
| 3                        | `*`                           | Cartesian Product                                    | `A * B`                     |
| 4                        | `&`                           | Intersection                                         | `A & B`                     |
| 5                        | `-`                           | Difference                                           | `A - B`                     |
| 6                        | `\|`                          | Union                                                | `A \| B`                     |
| 7                        | `^`, `with`                   | Metadata Operators                                   | `A ^ builtin::write`        |


## Expressions

```ebnf

EXPRESSION_BLOCK          ::= '{' (STATEMENT | EXPRESSION)* '}'

EXPRESSION_OPTIONAL_BLOCK ::= EXPRESSION_BLOCK | EXPRESSION

EXPRESSION                ::= EXPRESSION_RETURN

EXPRESSION_RETURN         ::= KEYWORD_RETURN EXPRESSION | EXPRESSION_IF

CONDITION_AND_BLOCK       ::= '(' EXPRESSION ')' EXPRESSION_OPTIONAL_BLOCK | EXPRESSION EXPRESSION_BLOCK

EXPRESSION_IF             ::= KEYWORD_IF CONDITION_AND_BLOCK
                            { KEYWORD_ELSE CONDITION_AND_BLOCK } 
                            | EXPRESSION_REFINE_CAST

EXPRESSION_REFINE_CAST    ::= KEYWORD_REFINE IDENTIFIER 
                              KEYWORD_WHEN EXPRESSION 
                              KEYWORD_ElSE EXPRESSION_OPTIONAL_BLOCK 
                            | EXPRESSION_LOGICAL_OR

EXPRESSION_LOGICAL_OR     ::= EXPRESSION_LOGICAL_AND { '||' EXPRESSION_LOGICAL_AND }

EXPRESSION_LOGICAL_AND    ::= EXPRESSION_BITWISE_OR { '&&' EXPRESSION_BITWISE_OR }

EXPRESSION_BITWISE_OR     ::= EXPRESSION_BITWISE_XOR { '|' EXPRESSION_BITWISE_XOR }

EXPRESSION_BITWISE_XOR    ::= EXPRESSION_BITWISE_AND { '^' EXPRESSION_BITWISE_AND }

EXPRESSION_BITWISE_AND    ::= EXPRESSION_EQUALITY { '&' EXPRESSION_EQUALITY }

EXPRESSION_EQUALITY       ::= EXPRESSION_RELATIONAL { ('==' | '!=' ) EXPRESSION_RELATIONAL }

EXPRESSION_RELATIONAL     ::= EXPRESSION_SHIFT { ('<' | '<=' | '>' | '>=' ) EXPRESSION_SHIFT }

EXPRESSION_SHIFT          ::= EXPRESSION_ADDITIVE { ( '<<' | '>>' ) EXPRESSION_ADDITIVE }

EXPRESSION_ADDITIVE       ::= EXPRESSION_MULTIPLICATIVE { ( '+' | '-' ) EXPRESSION_MULTIPLICATIVE }

EXPRESSION_MULTIPLICATIVE ::= EXPRESSION_CAST { ( '*' | '/' | '%' )  EXPRESSION_CAST }

EXPRESSION_CAST           ::= EXPRESSION_UNARY { KEYWORD_AS TYPE_EXPRESSION }

EXPRESSION_UNARY          ::= ( '-' | '!' | '~' | '*' | '&' | '&&' ) EXPRESSION_UNARY
                            | EXPRESSION_POSTFIX

FUNCTION_CALL_ARGUMENT    ::= { '.' IDENTIFIER '=' } EXPRESSION

EXPRESSION_POSTFIX        ::= EXPRESSION_SEQUENCE
                            {
                              EXPRESSION_FUNCTION_CALL
                            | EXPRESSION_INDEX
                            | EXPRESSION_ACCESS
                            } 
EXPRESSION_FUNCTION_CALL  ::= '(' { FUNCTION_CALL_ARGUMENT (',' FUNCTION_CALL_ARGUMENT)* {','} } ')'

EXPRESSION_INDEX          ::= '[' EXPRESSION ']'

EXPRESSION_ACCESS         ::= '.' IDENTIFIER


EXPRESSION_SEQUENCE       ::= EXPRESSION_INITIALIZER { '|>' EXPRESSION }

EXPRESSION_INITIALIZER    ::= '.' { '(' TYPE_EXPRESSION ')' } '{' (EXPRESSION | '.' IDENTIFIER '=' EXPRESSION) '}'
                            | EXPRESSION_PRIMARY

EXPRESSION_PRIMARY        ::= '(' EXPRESSION ')'
                            | EXPRESSION_BLOCK 
                            | EXPRESSION_TERMINAL

EXPRESSION_TERMINAL       ::= EXPRESSION_PATH
                            | EXPRESSION_LITERAL

EXPRESSION_PATH           ::= IDENTIFIER ( '::' IDENTIFIER )*

EXPRESSION_LITERAL        ::= INTEGER_LITERAL
                            | FLOAT_LITERAL 
                            | STRING_LITERAL 
                            | RAW_STRING_LITERAL 
                            | CHAR_LITERAL 
                            | BOOL_LITERAL
                            | SYMBOL

```

## Statements

```ebnf
STATEMENT             ::= DECLARATION | ASSIGNMENT | LOOP | TERMINATED_EXPRESSION

TERMINATED_EXPRESSION ::= EXPRESSION ';'

DECLARATION           ::= VARIABLE_DECLARATION | FUNCTION_DECLARATION | TYPE_DECLARATION

ASSIGNMENT            ::= EXPRESSION (
                          '='
                        | '+='
                        | '-='
                        | '*='
                        | '/='
                        | '%='
                        | '<<='
                        | '>>='
                        | '&='
                        | '^='
                        | '&&='
                        | '||='
                        ) EXPRESSION

LOOP                  ::= WHILE_LOOP | FOR_LOOP | KEYWORD_LOOP LOOP_BLOCK

WHILE_LOOP            ::= KEYWORD_WHILE EXPRESSION LOOP_BLOCK

FOR_LOOP              ::= KEYWORD_FOR IDENTIFIER KEYWORD_IN EXPRESSION LOOP_BLOCK

LOOP_BLOCK            ::= '{' (STATEMENT | LOOP_CONTINUE | LOOP_BREAK | EXPRESSION)* '}'

LOOP_CONTINUE         ::= KEYWORD_CONTINUE ';'

LOOP_BREAK            ::= KEYWORD_BREAK ';'

```


## Variables

```ebnf

BINDING              ::= IDENTIFIER {':' TYPE_EXPRESSION} '=' EXPRESSION

LET_BINDING          ::= KEYWORD_LET BINDING ';'

MUT_BINDING          ::= KEYWORD_MUT BINDING ';'

VARIABLE_DECLARATION ::= LET_BINDING | MUT_BINDING

```
 
## Types

> **NOTE**:
> The precedence level of TYPE\_EXPRESSION\_PRODUCT may be changed in the future

```ebnf
TYPE_DECLARATION                 ::= KEYWORD_TYPE IDENTIFIER '=' TYPE_EXPRESSION ';'

TYPE_EXPRESSION                  ::= TYPE_EXPRESSION_METADATA

TYPE_EXPRESSION_METADATA         ::= TYPE_EXPRESSION_STRICT_INCLUSION { ( '^' | KEYWORD_WITH ) TYPE_EXPRESSION_STRICT_INCLUSION }

TYPE_EXPRESSION_STRICT_INCLUSION ::= TYPE_EXPRESSION_INCLUSION { ( '<' | '>' ) TYPE_EXPRESSION_INCLUSION

TYPE_EXPRESSION_INCLUSION        ::= TYPE_EXPRESSION_MEMBERSHIP { ( '<=' | '>=' ) TYPE_EXPRESSION_MEMBERSHIP

TYPE_EXPRESSION_MEMBERSHIP       ::= TYPE_EXPRESSION_DIFFERENCE { KEYWORD_IN TYPE_EXPRESSION_DIFFERENCE }

TYPE_EXPRESSION_DIFFERENCE       ::= TYPE_EXPRESSION_UNION { '-' TYPE_EXPRESSION_UNION }

TYPE_EXPRESSION_UNION            ::= TYPE_EXPRESSION_INTERSECTION { '|' TYPE_EXPRESSION_INTERSECTION }

TYPE_EXPRESSION_INTERSECTION     ::= TYPE_EXPRESSION_PRODUCT { '&' TYPE_EXPRESSION_PRODUCT }

TYPE_EXPRESSION_PRODUCT          ::= TYPE_EXPRESSION_MODIFIERS { '*' TYPE_EXPRESSION_MODIFIERS }

TYPE_EXPRESSION_MODIFIERS        ::= ( '&' | '[]' | '[' INTEGER_LITERAL ']' )* TYPE_EXPRESSION_GROUPING

TYPE_EXPRESSION_GROUPING         ::= LAMBDA | '(' TYPE_EXPRESSION ')' | EXPRESSION_PATH | TYPE_EXPRESSION_LITERAL

TYPE_EXPRESSION_LABEL            ::= IDENTIFIER ':' TYPE_EXPRESSION_GROUPING


TYPE_EXPRESSION_LITERAL          ::= RANGE_LITERAL 
                                   | INTEGER_LITERAL 
                                   | FLOAT_LITERAL 
                                   | STRING_LITERAL 
                                   | CHAR_LITERAL 
                                   | BOOL_LITERAL 
                                   | TYPE_STRUCT_LITERAL
                                   | TYPE_ENUM_LITERAL
                                   | TYPE_IMPL_LITERAL
                                   | TYPE_EXPRESSION_LABEL
                                   | KEYWORD_SELF 
                                   | SYMBOL

TYPE_STRUCT_LITERAL              ::= KEYWORD_STRUCT '{' IDENTIFIER ':' TYPE_EXPRESSION (',' IDENTIFIER ':' TYPE_EXPRESSION)* {','} '}'

TYPE_ENUM_LITERAL                ::= KEYWORD_ENUM '{' (IDENTIFIER { ':' TYPE_EXPRESSION } (',' IDENTIFIER { ':' TYPE_EXPRESSION })*) '}'

TYPE_IMPL_LITERAL                ::= KEYWORD_IMPL '{' FUNCTION_DECLARATION+ '}'

```

## Functions

```ebnf

FUNCTION_MODIFIERS           ::= (KEYWORD_PUB | KEYWORD_EXPORT) (KEYWORD_INLINE | KEYWORD_PURE | KEYWORD_COMPTIME)*

FUNCTION_HEADER              ::= {FUNCTION_MODIFIERS} KEYWORD_FN IDENTIFIER

GENERIC                      ::= '$' IDENTIFIER

GENERIC_LIST                 ::= GENERIC (',' GENERIC) {','}

PARAMETER_LIST_INLINE        ::= {GENERIC_LIST} {IDENTIFIER ':' TYPE_EXPRESSION (',' IDENTIFIER ':' TYPE_EXPRESSION )* {','}}

PARAMETER_LIST_POSTFIX       ::= {GENERIC_LIST} {IDENTIFIER (',' IDENTIFIER) {','}}

FUNCTION_DECLARATION         ::= FUNCTION_DECLARATION_INLINE | FUNCTION_DECLARATION_POSTFIX

FUNCTION_DECLARATION_INLINE  ::= FUNCTION_HEADER '(' PARAMETER_LIST_INLINE ')' 
                                 {'->' TYPE_EXPRESSION}

FUNCTION_DECLARATION_POSTFIX ::= FUNCTION_HEADER '(' PARAMETER_LIST_POSTFIX ')' 
                               { ':' TYPE_EXPRESSION | '(' TYPE_EXPRESSION (',' TYPE_EXPRESSION)* {','} ')'} 
                               { '->' TYPE_EXPRESSION }

LAMBDA                       ::= '\\' ((GENERIC | IDENTIFIER {':' TYPE_EXPRESSION }) 
                               | '(' IDENTIFIER { ':' TYPE_EXPRESSION } (',' IDENTIFIER {':' TYPE_EXPRESSION })* {','} ')') 
                               { '->' TYPE_EXPRESSION } 
                               (EXPRESSION_BLOCK | EXPRESSION)

```
