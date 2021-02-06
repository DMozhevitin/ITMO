package MyPackage1;

import java.io.*;
import java.util.*;

public class Class1 {
    public static void main(String[] args) throws IOException {
        InputReader reader = new InputReader(new FileInputStream("automaton.in"));
//        InputReader reader = new InputReader(System.in);
        BufferedOutputStream out = new BufferedOutputStream(new FileOutputStream("automaton.out"));
//        BufferedOutputStream out = new BufferedOutputStream(System.out);

        Aut aut = new Aut(reader);

        int m = reader.nextInt();

        for (int i = 0; i < m; i++) {
            String word = reader.next();
            if (aut.isCorrectWord(word)) {
                out.write("yes\n".getBytes());
            } else {
                out.write("no\n".getBytes());
            }
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

    static class Aut {
        boolean found;
        char start;
        int size;
        List<List<Pair<Character, Character>>> edges;

        Aut(InputReader source) {
            found = false;
            size = source.nextInt();
            start = source.next().charAt(0);

            edges = new ArrayList<>();
            for (int i = 0; i < 30; i++) {
                edges.add(new ArrayList<>());
            }

            for (int i = 0; i < size; i++) {
                char from = source.next().charAt(0);
                source.next();
                String to = source.next();

                if (to.length() == 1) {
                    edges.get(from - 'A').add(new Pair<>('0', to.charAt(0)));
                } else {
                    edges.get(from - 'A').add(new Pair<>(to.charAt(1), to.charAt(0)));
                }
            }
        }

        boolean isCorrectWord(String word) {
            found = false;
            dfs(start, 0, word);
            return found;
        }

        void dfs(char v, int index, String word) {
            if (index == word.length() && v == '0') {
                found = true;
                return;
            }

            if (v == '0') {
                return;
            }

            if (index >= word.length()) {
                return;
            }

            char c = word.charAt(index);
            for (Pair<Character, Character> edge : edges.get(v - 'A')) {
                if (c == edge.value) {
                    dfs(edge.key, index + 1, word);
                }
            }
        }
    }
}

final class Pair<K, V>{
    public K key;
    public V value;

    public Pair(K key, V value) {
        this.key = key;
        this.value = value;
    }
}