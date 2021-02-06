package parser;

import expression.*;
import exception.*;
import operation.*;

public class ExpressionParser<T> implements Parser {
    private Token currentToken = Token.BEGIN;
    private String expression;
    private int index;
    private T value;
    private String variableName = "";
    private BinaryOperation<T> binaryOperation;
    private UnaryOperaion<T> unaryOperation;

    public ExpressionParser(BinaryOperation<T> binaryOperation, UnaryOperaion<T> unaryOperaion) {
        this.binaryOperation = binaryOperation;
        this.unaryOperation = unaryOperaion;
    }

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

    private String parseNumber() {
        StringBuilder number = new StringBuilder();
        while (index < expression.length() && (Character.isDigit(expression.charAt(index))
                || expression.charAt(index) == 'e' || expression.charAt(index) == '.')) {
            number.append(expression.charAt(index));
            index++;
        }

        index--;
        return number.toString();
    }

    private void getNextToken() throws ParsingException {
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
                            value = binaryOperation.parseValue("-" + number);
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

            default:
                if (c == 'x' || c == 'y' || c == 'z') {
                    currentToken = Token.VARIABLE;
                    variableName = String.valueOf(c);
                } else if (Character.isDigit(expression.charAt(index))) {
                    findOperation();
                    String number = parseNumber();

                    try {
                        value = binaryOperation.parseValue(number);
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

    private TripleExpression<T> unary() throws ParsingException {
        getNextToken();
        TripleExpression<T> result;

        switch (currentToken) {
            case CONST:
                result = new Const<T>(value);
                getNextToken();
                break;


            case VARIABLE:
                result = new Variable<T>(variableName);
                getNextToken();
                break;


            case MINUS:
                result = new Negate<T>(unary(), unaryOperation);
                break;


            case OPEN:
                result = AddSub();
                getNextToken();
                break;

            default:
                throw new ParsingException("Wrong expression :\n" + expression);

        }
        return result;
    }

    private TripleExpression<T> MulDiv() throws ParsingException {
        TripleExpression result = unary();
        while (currentToken == Token.MUL || currentToken == Token.DIV) {
            switch (currentToken) {
                case MUL:
                    result = new Multiply<T>(result, unary(), binaryOperation);
                    break;


                case DIV:
                    result = new Divide<T>(result, unary(), binaryOperation);
                    break;


                default:
                    return result;

            }
        }
        return result;
    }

    private TripleExpression<T> AddSub() throws ParsingException {
        TripleExpression<T> result = MulDiv();
        while (currentToken == Token.ADD || currentToken == Token.SUB) {
            switch (currentToken) {
                case ADD:
                    result = new Add<T>(result, MulDiv(), binaryOperation);
                    break;


                case SUB:
                    result = new Subtract<T>(result, MulDiv(), binaryOperation);
                    break;


                default:
                    return result;
            }
        }
        return result;
    }


    public TripleExpression<T> parse(String str) throws ParsingException {
        index = 0;
        expression = str;
        currentToken = Token.BEGIN;
        checkBrackets();
        return AddSub();
    }
}
