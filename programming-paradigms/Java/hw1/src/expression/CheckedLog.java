package expression;

import expression.exception.EvaluatingException;
import expression.exception.ImpossibleOperationException;

public class CheckedLog extends AbstractUnaryOperator {
    public CheckedLog(TripleExpression a) {
        super(a);
    }

    protected int operator(int x) throws EvaluatingException {
        check(x);
        return (int) Math.log(x);
    }

    protected void check(int x) throws ImpossibleOperationException {
        if (x <= 0) {
            throw new ImpossibleOperationException("Can't get log of not a positive number");
        }
    }
}
