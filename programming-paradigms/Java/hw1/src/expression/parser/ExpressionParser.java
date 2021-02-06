package expression.parser;

import expression.*;
import expression.exception.*;

public class ExpressionParser implements Parser {
    private Token currentToken = Token.BEGIN;
    private String expression;
    private int index = 0;
    private int value;
    private String variableName = "";

    private boolean isBinaryOperation(Token token) {
        return (token == Token.ADD || token == Token.SUB || token == Token.MUL || token == Token.DIV);
    }

    private void findOperand() throws MissingOperandException {
        if (currentToken == Token.BEGIN || isBinaryOperation(currentToken) || currentToken == Token.OPEN) {
            throw new MissingOperandException(expression, index);
        }
    }

    private void findOperation() throws MissingOperationException {
        if (currentToken == Token.CLOSE || currentToken == Token.CONST || currentToken == Token.VARIABLE) {
            throw new MissingOperationException(expression, index);
        }
    }

    private void checkBrackets() throws WrongCountOfBracketsException {
        int balance = 0;
        for (int i = 0; i < expression.length(); i++) {
            if (expression.charAt(i) == '(') {
                balance++;
            }

            if (expression.charAt(i) == ')') {
                balance--;
                if (balance < 0) {
                    throw new WrongCountOfBracketsException();
                }
            }
        }

        if (balance > 0) {
            throw new WrongCountOfBracketsException();
        }
    }

    private void skipWhiteSpace() {
        while (index < expression.length() && Character.isWhitespace(expression.charAt(index))) {
            index++;
        }
    }

    private String parseWord() {
        StringBuilder word = new StringBuilder();
        while (index < expression.length() && Character.isLetterOrDigit(expression.charAt(index))) {
            word.append(expression.charAt(index));
            index++;
        }

        index--;
        return word.toString();
    }

    private String parseNumber() {
        StringBuilder number = new StringBuilder();
        while (index < expression.length() && Character.isDigit(expression.charAt(index))) {
            number.append(expression.charAt(index));
            index++;
        }

        index--;
        return number.toString();
    }

    private void getNextToken() throws ParsingException {
        String word;
        skipWhiteSpace();
        if (index == expression.length()) {
            findOperand();
            currentToken = Token.END;
            return;
        }

        char c = expression.charAt(index);

        switch (c) {
            case '*':
                findOperand();
                currentToken = Token.MUL;
                break;


            case '/':
                findOperand();
                currentToken = Token.DIV;
                break;


            case '+':
                findOperand();
                currentToken = Token.ADD;
                break;


            case '-':
                if (currentToken == Token.VARIABLE || currentToken == Token.CONST || currentToken == Token.CLOSE) {
                    currentToken = Token.SUB;
                } else {
                    if (index == expression.length() - 1) {
                        throw new MissingOperandException(expression, index);
                    } else if (Character.isDigit(expression.charAt(index + 1))) {
                        index++;
                        String number = parseNumber();

                        try {
                            value = Integer.parseInt("-" + number);
                        } catch (NumberFormatException e) {
                            throw new BadConstException("-" + number + " is a wrong number");
                        }

                        currentToken = Token.CONST;
                    } else {
                        currentToken = Token.MINUS;
                    }
                }

                break;

            case '(':
                findOperation();
                currentToken = Token.OPEN;
                break;


            case ')':
                if (isBinaryOperation(currentToken) || currentToken == Token.OPEN) {
                    throw new MissingOperandException(expression, index);
                }
                currentToken = Token.CLOSE;
                break;

            case 'h' :
                word = parseWord();
                if (word.equals("high")) {
                    currentToken = Token.HIGH;
                    break;
                } else {
                    throw new UnknownSequenceException(word + " is an unknown sequence");
                }

            case 'l' :
                word = parseWord();
                if (word.equals("low")) {
                    currentToken = Token.LOW;
                    break;
                } else {
                    throw new UnknownSequenceException(word + " is an unknown sequence");
                }

            default:
                if (c == 'x' || c == 'y' || c == 'z') {
                    currentToken = Token.VARIABLE;
                    variableName = String.valueOf(c);
                } else if (Character.isDigit(expression.charAt(index))) {
                    findOperation();
                    String number = parseNumber();

                    try {
                        value = Integer.parseInt(number);
                    } catch (NumberFormatException e) {
                        throw new BadConstException(number + " is a wrong number");
                    }
                    currentToken = Token.CONST;
                } else {
                    throw new UnknownSequenceException(expression.charAt(index) + " is an unknown symbol");
                }

        }
        index++;
    }

    private TripleExpression unary() throws ParsingException {
        getNextToken();
        TripleExpression result;

        switch (currentToken) {
            case CONST:
                result = new Const(value);
                getNextToken();
                break;


            case VARIABLE:
                result = new Variable(variableName);
                getNextToken();
                break;


            case MINUS:
                result = new CheckedNegate(unary());
                break;


            case OPEN:
                result = AddSub();
                getNextToken();
                break;

            case HIGH:
                result = new High(unary());
                break;

            case LOW:
                result = new Low(unary());
                break;

            default:
                throw new ParsingException("Wrong expression :\n" + expression);

        }
        return result;
    }

    private TripleExpression MulDiv() throws ParsingException {
        TripleExpression result = unary();
        while (currentToken == Token.MUL || currentToken == Token.DIV) {
            switch (currentToken) {
                case MUL:
                    result = new CheckedMultiply(result, unary());
                    break;


                case DIV:
                    result = new CheckedDivide(result, unary());
                    break;


                default:
                    return result;

            }
        }
        return result;
    }

    private TripleExpression AddSub() throws ParsingException {
        TripleExpression result = MulDiv();
        while (currentToken == Token.ADD || currentToken == Token.SUB) {
            switch (currentToken) {
                case ADD:
                    result = new CheckedAdd(result, MulDiv());
                    break;


                case SUB:
                    result = new CheckedSubtract(result, MulDiv());
                    break;


                default:
                    return result;
            }
        }
        return result;
    }


    public TripleExpression parse(String str) throws ParsingException {
        index = 0;
        expression = str;
        currentToken = Token.BEGIN;
        checkBrackets();
        return AddSub();
    }
}
