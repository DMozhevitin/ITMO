package MyPackage1;

import java.io.*;
import java.util.*;

public class Class1 {
    public static void main(String[] args) throws Exception {
        final long P = (long) 1e9 + 7;
//        InputReader reader = new InputReader(System.in);
        InputReader reader = new InputReader(new FileInputStream("nfc.in"));
        BufferedOutputStream out = new BufferedOutputStream(new FileOutputStream("nfc.out"));
//        BufferedOutputStream out = new BufferedOutputStream(System.out);
//        BufferedReader bufreader = new BufferedReader(new InputStreamReader(System.in));
//        BufferedReader bufreader = new BufferedReader(new InputStreamReader(new FileInputStream("useless.in")));

        int n = reader.nextInt();
        long[][][] dp = new long[110][110][110];
        List<Pair<Character, String>> rules = new ArrayList<>();
        List<Pair<Character, String>> ntrules = new ArrayList<>();

        char start = reader.next().charAt(0);
        Set<Character> nt = new HashSet<>();
        for (int i = 0; i < n; i++) {
            char from = reader.next().charAt(0);
            nt.add(from);
            reader.next();
            String to = reader.next();

            for (int j = 0; j < to.length(); j++) {
                if (Character.isUpperCase(to.charAt(j))) {
                    nt.add(to.charAt(j));
                }
            }

            if (to.length() == 2) {
                ntrules.add(new Pair<>(from, to));
            }
            rules.add(new Pair<>(from, to));
        }

        String word = reader.next();

        for (int i = 0; i < word.length(); i++) {
            for (Pair<Character, String> rule : rules) {
                if (rule.value.length() == 1 && rule.value.charAt(0) == word.charAt(i)) {
                    dp[rule.key][i][i] = 1;
                }
            }
        }

        for (int len = 1; len < word.length(); len++) {
            for (int i = 0; i < word.length(); i++) {
                if (i + len < word.length()) {
                    for (int k = i; k < i + len; k++) {
                        for (Pair<Character, String> rule : ntrules) {
                            char B = rule.value.charAt(0);
                            char C = rule.value.charAt(1);
                            char A = rule.key;

                            dp[A][i][i + len] = (dp[A][i][i + len] + dp[B][i][k] * dp[C][k + 1][i + len]) % P;
                        }
                    }
                }
            }
        }

        out.write(Long.toString(dp[start][0][word.length() - 1]).getBytes());

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
}

final class Pair<K, V> {
    public K key;
    public V value;

    public Pair(K key, V value) {
        this.key = key;
        this.value = value;
    }
}
