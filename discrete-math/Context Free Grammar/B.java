package MyPackage1;

import java.io.*;
import java.util.*;

public class Class1 {
    public static void main(String[] args) throws Exception {
//        InputReader reader = new InputReader(new FileInputStream("epsilon.in"));
//        InputReader reader = new InputReader(System.in);
        BufferedOutputStream out = new BufferedOutputStream(new FileOutputStream("epsilon.out"));
//        BufferedOutputStream out = new BufferedOutputStream(System.out);
//        BufferedReader bufreader = new BufferedReader(new InputStreamReader(System.in));
        BufferedReader bufreader = new BufferedReader(new InputStreamReader(new FileInputStream("epsilon.in")));
        Aut aut = new Aut(bufreader);
        out.write(aut.findEpsilonPruductions().getBytes());

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
        List<List<String>> edges;
        Aut(BufferedReader source) throws IOException {
            found = false;
            String l = source.readLine();
            l = l.trim();
            int k = 0;
            StringBuilder sz = new StringBuilder();
            while (Character.isDigit(l.charAt(k))) {
                sz.append(l.charAt(k));
                k++;
            }

            size = Integer.parseInt(sz.toString());
            start = l.charAt(l.length() - 1);

            edges = new ArrayList<>();
            for (int i = 0; i < 100; i++) {
                edges.add(new ArrayList<>());
            }

            for (int i = 0; i < size; i++) {
                String line = source.readLine();

                if (line == null || line.isEmpty()) {
                    continue;
                }

                line = line.trim();

                char from = line.charAt(0);
                String to;
                if (line.charAt(line.length() - 1) == '>') {
                    to = "eps";
                } else {
                    to = line.substring(line.indexOf("->") + 3);
                }

                edges.get(from).add(to);
            }
        }

        String findEpsilonPruductions() throws Exception {
            Set<Character> set = new TreeSet<>();

            for (char i = 0; i < edges.size(); i++) {
                for (String to : edges.get(i)) {
                    if (to.equals("eps")) {
                        set.add(i);
                    }
                }
            }

            int cnt = 0;
            while (true) {
                boolean delta = false;
                cnt++;
                for (char i = 0; i < edges.size(); i++) {
                    for (String to : edges.get(i)) {
                        boolean b = true;
                        for (int j = 0; j < to.length(); j++) {
                            b &= set.contains(to.charAt(j));
                        }
                        if (b) {
                            int tmp = set.size();
                            set.add(i);
                            if (set.size() > tmp) {
                                delta = true;
                            }
                        }
                    }
                }

                if (!delta) {
                    break;
                }
            }

            StringBuilder epsilonProductions = new StringBuilder();
            for (Character c : set) {
                epsilonProductions.append(c).append(" ");
            }

            epsilonProductions.deleteCharAt(epsilonProductions.length() - 1);
            return epsilonProductions.toString();
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