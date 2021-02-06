package expression;

import exception.OverflowException;
import operation.UnaryOperaion;

public class Negate<T> extends AbstractUnaryOperator<T> {

    public Negate(TripleExpression a, UnaryOperaion<T> unaryOperation) {
        super(a, unaryOperation);
    }

    protected T operator(T x) throws OverflowException {
        return unaryOperation.negate(x);
    }

}
