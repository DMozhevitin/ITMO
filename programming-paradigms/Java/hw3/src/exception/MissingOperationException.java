package exception;

public class MissingOperationException extends ParsingException {

    public MissingOperationException(String expression, int index) {
        super("Missing operation:\n" + expression.substring(0, index) + "---> <---"
                + expression.substring(index));
    }
}
