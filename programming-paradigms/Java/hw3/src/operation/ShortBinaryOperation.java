package operation;


import exception.DivisionByZeroException;

public class ShortBinaryOperation implements BinaryOperation<Short> {
    public Short add(Short firstOperand, Short secondOperand) {
        return (short) (firstOperand + secondOperand);
    }

    public Short sub(Short firstOperand, Short secondOperand) {
        return (short) (firstOperand - secondOperand);
    }

    public Short mul(Short firstOperand, Short secondOperand) {
        return (short) (firstOperand * secondOperand);
    }

    public Short div(Short firstOperand, Short secondOperand) throws DivisionByZeroException {
        if (secondOperand == 0) {
            throw new DivisionByZeroException();
        }

        return (short) (firstOperand / secondOperand);
    }

    public Short parseValue(String value) {
        return (short) Integer.parseInt(value);
    }
}
