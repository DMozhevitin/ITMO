package expression;

import expression.exception.EvaluatingException;

public class Low extends AbstractUnaryOperator {
    public Low(TripleExpression a) {
        super(a);
    }

    protected int operator(int x) throws EvaluatingException {
        return Integer.lowestOneBit(x);
    }

}
