package operation;


public class LongUnaryOperation implements UnaryOperaion<Long> {
    public Long negate(Long operand) {
        return -operand;
    }
}
