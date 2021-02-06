package operation;

public class DoubleUnaryOperation implements UnaryOperaion<Double> {
    public Double negate(Double operand) {
        return -operand;
    }
}
