package expression.parser;

import expression.TripleExpression;

public class Test {
    public static void main(String[] args) {
        String s;
        s = "x * y * z";
        ExpressionParser p = new ExpressionParser();
        TripleExpression x = p.parse(s);
        System.out.println(x.evaluate(1, 2, 5));
        System.out.println("hui");
    }
}
