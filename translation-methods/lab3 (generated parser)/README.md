# Expressions in prefix notation parser

Translation from prefix exprassions into Python.

## Supported operations:
- arithmetic operations (`+, -, *, /`)
- logic operations (`&, |, ^, !`)
- comparison operations
- `if` statement
- variable assignment
- console output

## Example:

**Input**: `if > 1 2 = a 5 else if < 3 5 print * 100 500 else print b`

**Output**:
```
if 1 > 2:
    a = 5
elif 3 < 5:
    print(100 * 500)
else:
    print(b)
```
