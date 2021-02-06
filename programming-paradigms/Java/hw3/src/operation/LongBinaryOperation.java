package operation;

import exception.DivisionByZeroException;

public class LongBinaryOperation implements BinaryOperation<Long> {
    public Long add(Long firstOperand, Long secondOperand)  {
        return firstOperand + secondOperand;
    }

    public Long sub(Long firstOperand, Long secondOperand) {
        return firstOperand - secondOperand;
    }

    public Long mul(Long firstOperand, Long secondOperand) {
        return firstOperand * secondOperand;
    }

    public Long div(Long firstOperand, Long secondOperand) throws DivisionByZeroException {
        if (secondOperand == 0) {
            throw new DivisionByZeroException();
        }

        return firstOperand / secondOperand;
    }

    public Long parseValue(String value) {
        return Long.parseLong(value);
    }
}
