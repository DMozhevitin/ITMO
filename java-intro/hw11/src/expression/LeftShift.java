package expression;

public class LeftShift extends AbstractBinaryOperator{

    public LeftShift(TripleExpression a, TripleExpression b) {
        super(a, b);
    }

    protected int operator(int a, int b) {
        return a << b;
    }
}
