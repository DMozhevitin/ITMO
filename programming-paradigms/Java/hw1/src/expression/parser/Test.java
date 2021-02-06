package expression.parser;

import expression.exception.EvaluatingException;
import expression.exception.ParsingException;
import expression.TripleExpression;

public class Test {
    public static void main(String[] args) throws ParsingException, EvaluatingException {
        String s = "(higg 1) ";
        ExpressionParser parser = new ExpressionParser();
        TripleExpression x = parser.parse(s);
        System.out.println(x.evaluate(0, 0, 0));
    }
}
