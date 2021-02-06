package expression;

import exception.DivisionByZeroException;
import exception.OverflowException;
import operation.BinaryOperation;

public class Divide<T> extends AbstractBinaryOperator<T> {
    public Divide(TripleExpression a, TripleExpression b, BinaryOperation<T> operation) {
        super(a, b, operation);
    }

    protected T operator(T a, T b) throws DivisionByZeroException, OverflowException {
        return binaryOperation.div(a, b);
    }

}
