package operation;

public class ByteUnaryOperation implements UnaryOperaion<Byte> {
    public Byte negate(Byte operand) {
        return (byte) -operand;
    }
}
