package expression;

public class CheckedAdd extends AbstractBinaryOperator {
    public CheckedAdd(TripleExpression a, TripleExpression b) {
        super(a, b);
    }

    protected int operator(int a, int b) {
        return a + b;
    }
}
