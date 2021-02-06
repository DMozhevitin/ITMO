package expression;

public abstract class AbstractUnaryOperator implements TripleExpression {
    private TripleExpression operand;

    public AbstractUnaryOperator(TripleExpression a) {
        operand = a;
    }

    protected abstract int operator(int x);

    public int evaluate(int x, int y, int z) {
        return operator(operand.evaluate(x, y, z));
    }
}
