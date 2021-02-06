package expression;

import expression.exception.EvaluatingException;

public class High extends AbstractUnaryOperator{

    public High(TripleExpression a) {
        super(a);
    }

    protected int operator(int x) throws EvaluatingException {
        return Integer.highestOneBit(x);
    }

}
