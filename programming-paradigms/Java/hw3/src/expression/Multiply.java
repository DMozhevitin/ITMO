package expression;

import exception.OverflowException;
import operation.BinaryOperation;

public class Multiply<T> extends AbstractBinaryOperator<T> {
    public Multiply(TripleExpression a, TripleExpression b, BinaryOperation<T> operation) {
        super(a, b, operation);
    }

    protected T operator(T a, T b) throws OverflowException {
        return binaryOperation.mul(a, b);
    }

}
