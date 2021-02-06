package expression.exception;

public class DivisionByZeroException extends EvaluatingException{
    public DivisionByZeroException() {
        super("Division by zero");
    }
}
