package operation;


public class ShortUnaryOperation implements UnaryOperaion<Short> {
    public Short negate(Short operand) {
        return (short) -operand;
    }
}
