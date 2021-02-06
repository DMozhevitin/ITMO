package expression;

import expression.exception.OverflowException;

public class CheckedMultiply extends AbstractBinaryOperator {
    public CheckedMultiply(TripleExpression a, TripleExpression b) {
        super(a, b);
    }

    protected int operator(int a, int b) throws OverflowException{
        check(a, b);
        return a * b;
    }

    protected void check(int a, int b) throws OverflowException {
        if (b < 0) {
            if (a > 0 && b < Integer.MIN_VALUE / a) { //ab < int_min; b < int_min / a
                throw new OverflowException();
            }

            if (a < 0 && b < Integer.MAX_VALUE / a) {  //ab > int_max; b < int_max / a
                throw new OverflowException();
            }
        }

        if (b > 0) {
            if (a > 0 && a > Integer.MAX_VALUE / b) { //ab > int_max; a > int_max/b
                throw new OverflowException();
            }

            if (a < 0 && a < Integer.MIN_VALUE / b) { //ab < int_min; a < int_min / b
                throw new OverflowException();
            }
        }
    }

}
