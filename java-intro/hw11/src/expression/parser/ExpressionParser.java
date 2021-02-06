package expression.parser;

import expression.*;

public class ExpressionParser implements Parser {
    private Token current = Token.BEGIN;
    private String expression;
    private int index = 0, val;
    private char varName;

    private void skipWhiteSpace() {
        while (index < expression.length() && Character.isWhitespace(expression.charAt(index))) {
            index++;
        }
    }

    private void getNextToken() {
        skipWhiteSpace();
        if (index == expression.length()) {
            current = Token.END;
            return;
        }

        char c = expression.charAt(index);

        switch (c) {
            case '*': {
                current = Token.MUL;
                break;
            }

            case '/': {
                current = Token.DIV;
                break;
            }

            case '+': {
                current = Token.ADD;
                break;
            }

            case '-': {
                if (current == Token.VARIABLE || current == Token.CONST || current == Token.CLOSE) {
                    current = Token.SUB;
                } else {
                    current = Token.MINUS;
                }

                break;
            }

            case '(': {
                current = Token.OPEN;
                break;
            }

            case ')': {
                current = Token.CLOSE;
                break;
            }

            case '<': {
                current = Token.LEFT_SHIFT;
                index += 2;
                break;
            }

            case '>': {
                current = Token.RIGHT_SHIFT;
                index += 2;
                break;
            }

            default: {
                if (c == 'x' || c == 'y' || c == 'z') {
                    current = Token.VARIABLE;
                    varName = c;
                } else {
                    boolean b = false;
                    val = 0;
                    while (index < expression.length() && Character.isDigit(expression.charAt(index))) {
                        val = val * 10 + (expression.charAt(index) - '0');
                        index++;
                        b = true;
                    }
                    if (b) {
                        index--;
                        current = Token.CONST;
                    }
                }
            }
        }
        index++;
    }

    private TripleExpression unary() {
        getNextToken();
        TripleExpression result;

        switch (current) {
            case CONST: {
                result = new Const(val);
                getNextToken();
                break;
            }

            case VARIABLE: {
                result = new Variable(varName);
                getNextToken();
                break;
            }

            case MINUS: {
                result = new UnaryMinus(unary());
                break;
            }

            case OPEN: {
                result = shifts();
                getNextToken();
                break;
            }

            default: {
                return new Const(0);
            }
        }
        return result;
    }

    private TripleExpression MulDiv() {
        TripleExpression result = unary();
        while (current == Token.MUL || current == Token.DIV) {
            switch (current) {
                case MUL: {
                    result = new Multiply(result, unary());
                    break;
                }

                case DIV: {
                    result = new Divide(result, unary());
                    break;
                }

                default: {
                    return result;
                }
            }
        }
        return result;
    }

    private TripleExpression AddSub() {
        TripleExpression result = MulDiv();
        while (current == Token.ADD || current == Token.SUB) {
            switch (current) {
                case ADD: {
                    result = new Add(result, MulDiv());
                    break;
                }

                case SUB: {
                    result = new Subtract(result, MulDiv());
                    break;
                }

                default:
                    return result;
            }
        }
        return result;
    }

    private TripleExpression shifts() {
        TripleExpression result = AddSub();
        while (current == Token.LEFT_SHIFT || current == Token.RIGHT_SHIFT) {
            switch (current) {
                case LEFT_SHIFT: {
                    result = new LeftShift(result, AddSub());
                    break;
                }

                case RIGHT_SHIFT: {
                    result = new RightShift(result, AddSub());
                    break;
                }

                default: {
                    return result;
                }
            }
        }
        return result;
    }

    public TripleExpression parse(String str) {
        index = 0;
        expression = str;
        current = Token.BEGIN;
        return shifts();
    }
}
