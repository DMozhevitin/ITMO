package expression;

import exception.EvaluatingException;
import operation.BinaryOperation;

public abstract class AbstractBinaryOperator<T> implements TripleExpression<T> {
    private TripleExpression<T> first, second;
    protected BinaryOperation<T> binaryOperation;

    public AbstractBinaryOperator(TripleExpression<T> first, TripleExpression<T> second,
                                  BinaryOperation<T> binaryOperation) {
        this.first = first;
        this.second = second;
        this.binaryOperation = binaryOperation;
    }

    protected abstract T operator(T a, T b) throws EvaluatingException;

    public T evaluate(T x, T y, T z) throws EvaluatingException {
        return operator(first.evaluate(x, y, z), second.evaluate(x, y, z));
    }
}
