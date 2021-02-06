package operation;

import java.math.BigInteger;

public class BigIntegerUnaryOperation implements UnaryOperaion<BigInteger> {
    public BigInteger negate(BigInteger operand) {
        return operand.negate();
    }
}
