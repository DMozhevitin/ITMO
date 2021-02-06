package expression;

public class Divide extends AbstractBinaryOperator {
    public Divide(TripleExpression a, TripleExpression b) {
        super(a, b);
    }

    protected int operator(int a, int b) {
        return a / b;
    }
}
