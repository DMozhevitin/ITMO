package parser;

import exception.ParsingException;
import expression.TripleExpression;

public interface Parser<T> {
    TripleExpression<T> parse(String expression) throws ParsingException;
}
