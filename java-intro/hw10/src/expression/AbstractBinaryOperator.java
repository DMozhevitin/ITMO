package expression;

public abstract class AbstractBinaryOperator implements DoubleExpression {
    private final DoubleExpression firstOperand;
    private final DoubleExpression secondOperand;

    public AbstractBinaryOperator(DoubleExpression x, DoubleExpression y) {
        firstOperand = x;
        secondOperand = y;
    }

    protected abstract double apply(double x, double y);

    public double evaluate(double x) {
        return apply(firstOperand.evaluate(x), secondOperand.evaluate(x));
    }

}