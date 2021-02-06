import java.math.BigInteger;
import java.sql.SQLOutput;
import java.util.Scanner;

public class Task {
    public static void main(String[] args) {
        long w, h, n;
        final int MAXN = 100_000;
        Scanner scan = new Scanner(System.in);
        w = scan.nextInt();
        h = scan.nextInt();
        n = scan.nextInt();

        BigInteger W = BigInteger.valueOf(w);
        BigInteger H = BigInteger.valueOf(h);
        BigInteger N = BigInteger.valueOf(n);

        long left = 0, right = n * Math.max(w, h) + 1;
        BigInteger Left = BigInteger.valueOf(left);
        BigInteger Right = new BigInteger("1000000000000000000");

        for (int i = 0; i < MAXN; i++) {
            BigInteger Mid = Left.add(Right);
            BigInteger Two = BigInteger.valueOf(2);
            Mid = Mid.divide(Two);

            BigInteger Res = Mid.divide(W);
            Res = Res.multiply(Mid.divide(H));

            int comp = Res.compareTo(N);
            if (comp < 0) {
                Left = Mid;
            } else {
                Right = Mid;
            }
        }

        System.out.println(Right.toString());
    }
}