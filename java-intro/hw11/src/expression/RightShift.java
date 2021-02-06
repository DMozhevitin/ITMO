package expression;

public class RightShift extends AbstractBinaryOperator {
    public RightShift(TripleExpression a, TripleExpression b) {
        super(a, b);
    }

    protected int operator(int a, int b) {
        return a >> b;
    }
}
