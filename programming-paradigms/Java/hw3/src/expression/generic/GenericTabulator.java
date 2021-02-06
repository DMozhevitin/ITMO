package expression.generic;

import exception.EvaluatingException;
import exception.IncorrectModeException;
import exception.ParsingException;
import expression.TripleExpression;
import operation.*;
import parser.ExpressionParser;

import java.util.HashMap;
import java.util.Map;

public class GenericTabulator implements Tabulator {
    private BinaryOperation binaryOperation;
    private UnaryOperaion unaryOperaion;

    private HashMap<String, Map.Entry<BinaryOperation, UnaryOperaion>> modes = new HashMap<>();

    public GenericTabulator() {
        modes.put("i", Map.entry(new IntegerBinaryOperation(true),
                new IntegerUnaryOperation(true)));

        modes.put("u", Map.entry(new IntegerBinaryOperation(false),
                new IntegerUnaryOperation(false)));

        modes.put("d", Map.entry(new DoubleBinaryOperation(), new DoubleUnaryOperation()));
        modes.put("bi", Map.entry(new BigIntegerBinaryOperation(), new BigIntegerUnaryOperation()));
        modes.put("f", Map.entry(new FloatBinaryOperation(), new FloatUnaryOperation()));
        modes.put("b", Map.entry(new ByteBinaryOperation(), new ByteUnaryOperation()));
        modes.put("l", Map.entry(new LongBinaryOperation(), new LongUnaryOperation()));
        modes.put("s", Map.entry(new ShortBinaryOperation(), new ShortUnaryOperation()));
    }

    public Object[][][] tabulate(String mode, String expression, int x1, int x2, int y1, int y2, int z1, int z2) throws Exception {
        if (modes.get(mode) != null) {
            binaryOperation = modes.get(mode).getKey();
            unaryOperaion = modes.get(mode).getValue();
        } else {
            throw new IncorrectModeException(mode);
        }

        return fillTable(binaryOperation, unaryOperaion, expression, x1, x2, y1, y2, z1, z2);
    }

    private <T> Object[][][] fillTable(BinaryOperation<T> binaryOperation, UnaryOperaion<T> unaryOperation,
                                       String expression, int x1, int x2, int y1, int y2, int z1, int z2) {
        ExpressionParser<T> parser = new ExpressionParser<>(binaryOperation, unaryOperation);
        Object[][][] result = new Object[x2 - x1 + 1][y2 - y1 + 1][z2 - z1 + 1];
        TripleExpression<T> tripleExpression;
        try {
            tripleExpression = parser.parse(expression);
        } catch (ParsingException e) {
            return null;
        }

        for (int x = x1; x <= x2; x++) {
            for (int y = y1; y <= y2; y++) {
                for (int z = z1; z <= z2; z++) {
                    T i = binaryOperation.parseValue(Integer.toString(x));
                    T j = binaryOperation.parseValue(Integer.toString(y));
                    T k = binaryOperation.parseValue(Integer.toString(z));

                    try {
                        result[x - x1][y - y1][z - z1] = tripleExpression.evaluate(i, j, k);
                    } catch (EvaluatingException e) {
                        result[x - x1][y - y1][z - z1] = null;
                    }
                }
            }
        }

        return result;
    }

}
