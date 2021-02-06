package expression;

public class Subtract extends AbstractBinaryOperator{

    public Subtract(TripleExpression a, TripleExpression b) {
        super(a, b);
    }

    protected int operator(int a, int b) {
        return a - b;
    }
}
