package expression;

import expression.exception.EvaluatingException;

public abstract class AbstractBinaryOperator implements TripleExpression {
    private TripleExpression first, second;


    public AbstractBinaryOperator(TripleExpression first, TripleExpression second) {
        this.first = first;
        this.second = second;
    }

    protected abstract int operator(int a, int b) throws EvaluatingException;

    protected abstract void check(int a, int b) throws EvaluatingException;

    public int evaluate(int x, int y, int z) throws EvaluatingException{
        return operator(first.evaluate(x, y, z), second.evaluate(x, y, z));
    }
}
