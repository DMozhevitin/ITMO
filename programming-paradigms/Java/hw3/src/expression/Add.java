package expression;

import exception.OverflowException;
import operation.BinaryOperation;

public class Add<T> extends AbstractBinaryOperator<T> {
    public Add(TripleExpression<T> a, TripleExpression<T> b, BinaryOperation<T> operation) {
        super(a, b, operation);
    }

    protected T operator(T a, T b) throws OverflowException {
        return binaryOperation.add(a, b);
    }
}
