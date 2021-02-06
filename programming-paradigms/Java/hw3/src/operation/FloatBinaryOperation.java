package operation;


public class FloatBinaryOperation implements BinaryOperation<Float> {
    public Float add(Float firstOperand, Float secondOperand) {
        return firstOperand + secondOperand;
    }

    public Float sub(Float firstOperand, Float secondOperand) {
        return firstOperand - secondOperand;
    }

    public Float mul(Float firstOperand, Float secondOperand) {
        return firstOperand * secondOperand;
    }

    public Float div(Float firstOperand, Float secondOperand) {
        return firstOperand / secondOperand;
    }

    public Float parseValue(String value) {
        return Float.parseFloat(value);
    }
}
