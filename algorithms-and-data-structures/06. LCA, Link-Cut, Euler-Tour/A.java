import java.io.*;
import java.util.*;

public class Task {
    public static void main(String[] args) throws IOException {
        InputReader reader = new InputReader(System.in);
        BufferedOutputStream out = new BufferedOutputStream(System.out);
        int n = reader.nextInt();
        int[] parent = new int[n + 1];
        int[][] dp = new int[n + 1][50];

        for (int i = 1; i <= n; i++) {
            parent[i] = reader.nextInt();
        }

        for (int i = 1; i <= n; i++) {
            dp[i][0] = parent[i];
        }

        int l = (int)(Math.log(n) / Math.log(2));
        for (int j = 1; j <= l; j++) {
            for (int i = 1; i <= n; i++) {
                dp[i][j] = dp[dp[i][j - 1]][j - 1];
            }
        }

        for (int i = 1; i <= n; i++) {
            out.write(Integer.toString(i).getBytes());
            out.write(": ".getBytes());
            for (int j = 0; j <= l; j++) {
                if (dp[i][j] == 0) {
                    break;
                }
                out.write(Integer.toString(dp[i][j]).getBytes());
                out.write(" ".getBytes());
            }
            out.write("\n".getBytes());
        }

        out.close();
    }

    static class InputReader {
        public BufferedReader reader;
        public StringTokenizer tokenizer;

        public InputReader(InputStream stream) {
            reader = new BufferedReader(new InputStreamReader(stream), 32768);
            tokenizer = null;
        }

        public String next() {
            while (tokenizer == null || !tokenizer.hasMoreTokens()) {
                try {
                    tokenizer = new StringTokenizer(reader.readLine());
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            }
            return tokenizer.nextToken();
        }

        public int nextInt() {
            return Integer.parseInt(next());
        }

    }

    public static int log2(int n) {
        if (n == 0) {
            return 0;
        }

        return log2(n / 2) + 1;
    }
}