package expression;

import expression.exception.EvaluatingException;

public abstract class AbstractUnaryOperator implements TripleExpression {
    private TripleExpression operand;

    public AbstractUnaryOperator(TripleExpression operand) {
        this.operand = operand;
    }

    protected abstract int operator(int x) throws EvaluatingException;

    public int evaluate(int x, int y, int z)throws EvaluatingException {
        return operator(operand.evaluate(x, y, z));
    }
}
