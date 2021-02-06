package expression;

import expression.exception.OverflowException;

public class CheckedAdd extends AbstractBinaryOperator {
    public CheckedAdd(TripleExpression a, TripleExpression b) {
        super(a, b);
    }

    protected int operator(int a, int b) throws OverflowException{
        check(a, b);
        return a + b;
    }

    protected void check(int a, int b) throws OverflowException{
        if (a > 0 && b > Integer.MAX_VALUE - a) { //a + b > int_max;b > int_max - a;
            throw new OverflowException();
        }

        if (a < 0 && b < Integer.MIN_VALUE - a) { //a + b < int_min; b < int_min - a;
            throw new OverflowException();
        }
    }
}
