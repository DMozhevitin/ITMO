package expression;

public class LeftShift extends AbstractBinaryOperator{

    public LeftShift(TripleExpression a, TripleExpression b) {
        super(a, b);
    }

    protected void check(int a, int b) {

    }

    protected int operator(int a, int b) {
        return a << b;
    }
}
