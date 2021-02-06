package expression;

public class Const implements TripleExpression {
    private int value;

    public Const(int x) {
        value = x;
    }

    public int evaluate(int x, int y, int z) {
        return value;
    }
}
