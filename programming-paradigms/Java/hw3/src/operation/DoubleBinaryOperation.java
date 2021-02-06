package operation;

public class DoubleBinaryOperation implements BinaryOperation<Double> {
    public Double add(Double firstOperand, Double secondOperand) {
        return firstOperand + secondOperand;
    }

    public Double sub(Double firstOperand, Double secondOperand) {
        return firstOperand - secondOperand;
    }

    public Double mul(Double firstOperand, Double secondOperand) {
        return firstOperand * secondOperand;
    }

    public Double div(Double firstOperand, Double secondOperand) {
        return firstOperand / secondOperand;
    }

    public Double parseValue(String value) throws NumberFormatException {
        return Double.parseDouble(value);
    }
}
