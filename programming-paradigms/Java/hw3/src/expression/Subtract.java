package expression;

import exception.DivisionByZeroException;
import exception.OverflowException;
import operation.BinaryOperation;

public class Subtract<T> extends AbstractBinaryOperator<T> {

    public Subtract(TripleExpression a, TripleExpression b, BinaryOperation<T> operation) {
        super(a, b, operation);
    }

    protected T operator(T a, T b) throws OverflowException, DivisionByZeroException {
        return binaryOperation.sub(a, b);
    }


}
