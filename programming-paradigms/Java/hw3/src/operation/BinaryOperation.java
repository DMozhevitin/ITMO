package operation;

import exception.DivisionByZeroException;
import exception.OverflowException;

public interface BinaryOperation<T> {
    T add(T firstOperand, T secondOperand) throws OverflowException;

    T sub(T firstOperand, T secondOperand) throws DivisionByZeroException, OverflowException;

    T mul(T firstOperand, T secondOperand) throws OverflowException;

    T div(T firstOperand, T secondOperand) throws DivisionByZeroException, OverflowException;

    T parseValue(String value);
}
