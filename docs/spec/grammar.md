# Formal Grammar

This is the formal grammar for the _syntax_ of the Conical programming language.
It uses an extended Backus-Naur form (EBNF) with the addition of negation (`~ expression`) and unicode values (`U+XXXXXX`)

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
KEYWORD_MACRO    ::= 'macro'
KEYWORD_DO       ::= 'do'
KEYWORD_PURE     ::= 'pure'
KEYWORD_TRUE     ::= 'true'
KEYWORD_FALSE    ::= 'false'
```

## Escape Sequences

These are the valid escape sequences for strings and characters:
```ebnf
ESCAPE_SEQUENCE ::= 
        '\' ([0ntrvfb'"\] 
        | 'x' {HEX_DIGIT, 2} 
        | 'u' {HEX_DIGIT, 4}
        | 'U' {HEX_DIGIT, 8}
UNICODE_NON_CHARACTERS ::=
          [U+00FDD0-U+00FDEF] 
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
DISALLOWED_CHARACTERS ::=
                          [U+000000-U+00001F] 
                        | [U+00007F]
                        | [U+000080-U+00009F] 
                        | [U+00D800-U+00DFFF] 
                        | UNICODE_NON_CHARACTERS

BOOL                  ::= KEYWORD_TRUE | KEYWORD_FALSE
CHAR                  ::= '\'' (~ DISALLOWED_CHARACTERS) | ESCAPE_SEQUENCE '\''
STRING                ::= '"' ( (~ DISALLOWED_CHARACTERS) | ESCAPE_SEQUENCE )* '"'
RAW_STRING            ::= '`' (~ DISALLOWED_CHARACTERS)* | [U+00000A] | [U+00000D] '`'

SIGN                  ::= [+-]
INTEGER               ::= {SIGN} DECIMAL_LITERAL | BINARY_LITERAL | OCTAL_LITERAL | HEX_LITERAL
DECIMAL_LITERAL       ::= (DIGIT_ONE | '_') (DIGIT | '_')*
BINARY_LITERAL        ::= '0' [bB] (BINARY_DIGIT | '_')+
OCTAL_LITERAL         ::= '0' [oO] (OCTAL_DIGIT | '_')+
HEX_LITERAL           ::= '0' [xX] (HEX_DIGIT | '_')+

FLOAT_LITERAL ::= {SIGN} DECIMAL_LITERAL ( '.' DECIMAL_LITERAL | '.' DECIMAL_LITERAL [eE] {SIGN} DIGIT*)
```
## Operators

Here is the precedence table for operators:

| Precedence (High → Low)  | Operator(s)                   | Description                                         | Example                 |
| -------------------------|-------------------------------|-----------------------------------------------------|-------------------------|
| 1                        | `()`                          | Parentheses                                         | `(a+b)*c`               |
| 2                        | `!`, `~`, `+`, `-`, `++`, `-  | Logical NOT, Bitwise NOT, Unary plus/minus          | `!x`, `~x`, `-x`        |
| 4                        | `*`, `/`, `%`                 | Multiply, Divide, Modulus                           | `a*b`, `a/b`, `a%b`     |
| 5                        | `+`, `-`                      | Add, Subtract                                       | `a+b`, `a-b`            |
| 6                        | `<<`, `>>`                    | Bit shifts                                          | `x<<1`, `x>>1`          |
| 7                        | `<`, `<=`, `>`, `>=`          | Relational comparison                               | `a < b`                 |
| 8                        | `==`, `!=`                    | Equality                                            | `a == b`                |
| 9                        | `&`                           | Bitwise AND                                         | `a & b`                 |
| 10                       | `^`                           | Bitwise XOR                                         | `a ^ b`                 |
| 11                       | `|`                           | Bitwise OR                                          | `a | b`                 |
| 12                       | `&&`                          | Logical AND                                         | `a && b`                |
| 13                       | `||`                          | Logical OR                                          | `a || b`                |
| 14                       | `=`, `+=`, `-=`, `*=`, `/=`   | Assignment operators                                | `x += 3`                |



