# C-style logic expressions parser.

## Operations:
- `&` - conjunction
- `|` - disjunction
- `^` - exclusive or
- `!` - negation

Parenthesis can be used to change priorities.
.
Variables of one letter used as operands. One terminal is enough to describe variables, but for each logical operation there must be its own terminal.

## Example:

`(!a | b) & a & (a | !(b ^ c))`
