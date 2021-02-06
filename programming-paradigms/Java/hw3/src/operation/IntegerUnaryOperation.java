package operation;

import exception.OverflowException;

public class IntegerUnaryOperation implements UnaryOperaion<Integer> {
    private boolean checkedOperation;

    public IntegerUnaryOperation(boolean checkedOperation) {
        this.checkedOperation = checkedOperation;
    }

    public Integer negate(Integer operand) throws OverflowException {
        if (checkedOperation) {
            checkNegate(operand);
        }

        return -operand;
    }

    private void checkNegate(Integer operand) throws OverflowException {
        if (operand == Integer.MIN_VALUE) {
            throw new OverflowException();
        }
    }
}
