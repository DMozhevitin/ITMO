package expression;

import expression.exception.EvaluatingException;

public interface TripleExpression {
    int evaluate(int x, int y, int z) throws EvaluatingException;
}
