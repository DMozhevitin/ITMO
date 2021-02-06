package expression.exception;

public class OverflowException extends EvaluatingException{
    public OverflowException() {
        super("Integer is overflowed");
    }
}
