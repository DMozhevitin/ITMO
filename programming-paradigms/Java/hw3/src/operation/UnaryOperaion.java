package operation;

import exception.OverflowException;

public interface UnaryOperaion<T> {
    T negate(T operand) throws OverflowException;
}
