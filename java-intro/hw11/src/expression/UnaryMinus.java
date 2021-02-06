package expression;

public class UnaryMinus extends AbstractUnaryOperator{

    public UnaryMinus(TripleExpression a) {
        super(a);
    }

    protected int operator(int x) {
        return -x;
    }
}
