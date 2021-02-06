package expression.exception;

public class MissingOperandException extends ParsingException {
    public MissingOperandException(String expression, int index) {
        super("Missing operand:\n " + expression.substring(0, index) + "---> <---"
                + expression.substring(index));

    }
}
