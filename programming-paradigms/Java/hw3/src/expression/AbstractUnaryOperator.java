package expression;

import exception.EvaluatingException;
import operation.UnaryOperaion;

public abstract class AbstractUnaryOperator<T> implements TripleExpression<T> {
    private TripleExpression<T> operand;
    protected UnaryOperaion<T> unaryOperation;

    public AbstractUnaryOperator(TripleExpression operand, UnaryOperaion<T> unaryOperaion) {
        this.operand = operand;
        this.unaryOperation = unaryOperaion;
    }

    protected abstract T operator(T x) throws EvaluatingException;

    public T evaluate(T x, T y, T z) throws EvaluatingException {
        return operator(operand.evaluate(x, y, z));
    }
}
