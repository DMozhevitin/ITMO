package expression;

public class Multiply extends AbstractBinaryOperator {
    public Multiply(TripleExpression a, TripleExpression b) {
        super(a, b);
    }

    protected int operator(int a, int b) {
        return a * b;
    }
}
