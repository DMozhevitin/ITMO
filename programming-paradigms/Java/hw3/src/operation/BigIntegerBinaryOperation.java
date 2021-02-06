package operation;

import exception.DivisionByZeroException;

import java.math.BigInteger;

public class BigIntegerBinaryOperation implements BinaryOperation<BigInteger> {
    public BigInteger add(BigInteger firstOperand, BigInteger secondOperand) {
        BigInteger result = new BigInteger(firstOperand.toString());
        return result.add(secondOperand);
    }

    public BigInteger sub(BigInteger firstOperand, BigInteger secondOperand) {
        BigInteger result = new BigInteger(firstOperand.toString());
        return result.subtract(secondOperand);
    }

    public BigInteger mul(BigInteger firstOperand, BigInteger secondOperand) {
        BigInteger result = new BigInteger(firstOperand.toString());
        return result.multiply(secondOperand);
    }

    public BigInteger div(BigInteger firstOperand, BigInteger secondOperand) throws DivisionByZeroException {
        if (secondOperand.equals(BigInteger.ZERO)) {
            throw new DivisionByZeroException();
        }

        BigInteger result = new BigInteger(firstOperand.toString());
        return result.divide(secondOperand);
    }

    public BigInteger parseValue(String value) throws NumberFormatException {
        return new BigInteger(value);
    }
}
