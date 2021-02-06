package expression;

import expression.exception.OverflowException;

public class CheckedNegate extends AbstractUnaryOperator{

    public CheckedNegate(TripleExpression a) {
        super(a);
    }

    protected int operator(int x) throws OverflowException{
        check(x);
        return -x;
    }

    protected void check(int x) throws OverflowException{
        if (x == Integer.MIN_VALUE) {
            throw new OverflowException();
        }
    }
}
