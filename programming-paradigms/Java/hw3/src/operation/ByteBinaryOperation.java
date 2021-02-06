package operation;


import exception.DivisionByZeroException;

public class ByteBinaryOperation implements BinaryOperation<Byte> {
    public Byte add(Byte firstOperand, Byte secondOperand) {
        return (byte) (firstOperand + secondOperand);
    }

    public Byte sub(Byte firstOperand, Byte secondOperand) {
        return (byte) (firstOperand - secondOperand);
    }

    public Byte mul(Byte firstOperand, Byte secondOperand) {
        return (byte) (firstOperand * secondOperand);
    }

    public Byte div(Byte firstOperand, Byte secondOperand) throws DivisionByZeroException {
        if (secondOperand == 0) {
            throw new DivisionByZeroException();
        }
        return (byte) (firstOperand / secondOperand);
    }

    public Byte parseValue(String value) {
        return (byte) Integer.parseInt(value);
    }
}
