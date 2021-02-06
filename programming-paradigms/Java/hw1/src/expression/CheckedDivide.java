package expression;

import expression.exception.DivisionByZeroException;
import expression.exception.OverflowException;

public class CheckedDivide extends AbstractBinaryOperator {
    public CheckedDivide(TripleExpression a, TripleExpression b) {
        super(a, b);
    }

    protected int operator(int a, int b) throws DivisionByZeroException, OverflowException{
        check(a, b);
        return a / b;
    }

    protected void check(int a, int b)throws DivisionByZeroException, OverflowException {
        if (a == Integer.MIN_VALUE && b == -1) {
            throw new OverflowException();
        }

        if (b == 0) {
            throw new DivisionByZeroException();
        }
    }
}
