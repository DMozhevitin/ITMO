package expression;

public abstract class AbstractBinaryOperator implements TripleExpression {
    private TripleExpression first, second;

    public AbstractBinaryOperator(TripleExpression a, TripleExpression b) {
        first = a;
        second = b;
    }

    protected abstract int operator(int a, int b);

    public int evaluate(int x, int y, int z) {
        return operator(first.evaluate(x, y, z), second.evaluate(x, y, z));
    }
}
