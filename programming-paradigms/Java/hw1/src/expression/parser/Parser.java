package expression.parser;

import expression.exception.ParsingException;
import expression.TripleExpression;

public interface Parser {
    TripleExpression parse(String expression) throws ParsingException;
}
