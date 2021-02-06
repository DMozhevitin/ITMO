package expression.exception;

public class WrongCountOfBracketsException extends ParsingException{
    public WrongCountOfBracketsException() {
        super("Use correct brackets");
    }
}
