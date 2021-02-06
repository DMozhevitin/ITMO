package MyPackage1;

import java.io.*;
import java.util.*;

public class Class1 {
    public static void main(String[] args) throws Exception {
//        InputReader reader = new InputReader(new FileInputStream("epsilon.in"));
//        InputReader reader = new InputReader(System.in);
        BufferedOutputStream out = new BufferedOutputStream(new FileOutputStream("useless.out"));
//        BufferedOutputStream out = new BufferedOutputStream(System.out);
//        BufferedReader bufreader = new BufferedReader(new InputStreamReader(System.in));
        BufferedReader bufreader = new BufferedReader(new InputStreamReader(new FileInputStream("useless.in")));
        Aut aut;
        try {
            aut = new Aut(bufreader);
        } catch (IndexOutOfBoundsException e) {
            while (true) {}
        }

        Set<Character> useless;
        aut.deleteUselessSymbols();
        useless = aut.getUselessSymbols();

        for (Character c : useless) {
            out.write((c + " ").getBytes());
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
        char start;
        int size;
        boolean[] was;
        Set<Character> useless;
        List<List<String>> edges;
        Set<Character> nonGenerating;
        Set<Character> unreachable;

        Aut(BufferedReader source) throws IOException {
            unreachable = new HashSet<>();
            nonGenerating = new HashSet<>();
            String l = source.readLine();
            while (l == null || l.isEmpty()) {
                l = source.readLine();
            }
            l = l.trim();
            size = parseSize(l);
            start = l.charAt(l.length() - 1);
            useless = new TreeSet<>();
            was = new boolean[200];
            was[start] = true;
            edges = new ArrayList<>();
            for (int i = 0; i < 200; i++) {
                edges.add(new ArrayList<>());
            }

            for (int i = 0; i < size; i++) {
                String line = source.readLine();

                line = line.trim();

                char from = line.charAt(0);
                was[from] = true;
                String to;
                if (line.charAt(line.length() - 1) == '>') {
                    to = "eps";
                } else {
                    to = line.substring(line.indexOf("->") + 3);
                    for (int j = 0; j < to.length(); j++) {
                        was[to.charAt(j)] = true;
                    }
                }

                edges.get(from).add(to);
            }
        }

        public Set<Character> getUselessSymbols() {
            return useless;
        }

        public void deleteUselessSymbols() {
            deleteNonGeneratingSymbols();
            deleteUnreachableSymbols();
            useless.addAll(nonGenerating);
            useless.addAll(unreachable);
        }

        private void deleteNonGeneratingSymbols() {
            Set<Character> generating = new HashSet<>();

            //step 1
            for (char i = 0; i < edges.size(); i++) {
                for (String right : edges.get(i)) {
                    boolean b = true;
                    for (int j = 0; j < right.length(); j++) {
                        b &= (Character.isLowerCase(right.charAt(j)));
                    }
                    if (b) {
                        generating.add(i);
                    }
                }
            }

            //step 2
            while (true) {
                boolean delta = false;
                for (char i = 0; i < edges.size(); i++) {
                    for (String right : edges.get(i)) {
                        boolean b = true;
                        for (int j = 0; j < right.length(); j++) {
                            if (Character.isUpperCase(right.charAt(j))) {
                                b &= (generating.contains(right.charAt(j)));
                            }
                        }
                        if (b) {
                            int tmp = generating.size();
                            generating.add(i);
                            if (generating.size() > tmp) {
                                delta = true;
                            }
                        }
                    }
                }

                if (!delta) {
                    break;
                }
            }

            List<List<String>> newEdges = new ArrayList<>();
            for (int i = 0; i < edges.size(); i++) {
                newEdges.add(new ArrayList<>());
            }

            for (char i = 0; i < edges.size(); i++) {
                if (was[i] && Character.isUpperCase(i) && !generating.contains(i)) {
                    continue;
                }

                loop:
                for (String to : edges.get(i)) {
                    for (int j = 0; j < to.length(); j++) {
                        if (Character.isUpperCase(to.charAt(j)) && !generating.contains(to.charAt(j))) {
                            continue loop;
                        }
                    }
                    newEdges.get(i).add(to);
                }
            }

            edges = new ArrayList<>(newEdges);

            for (char c = 0; c < 100; c++) {
                if (Character.isUpperCase(c) && was[c] && !generating.contains(c)) {
                    nonGenerating.add(c);
                }
            }
        }

        private void deleteUnreachableSymbols() {
            if (nonGenerating.contains(start)) {
                for (char c = 65; c <= 90; c++) {
                    if (was[c]) {
                        unreachable.add(c);
                    }
                }
                return;
            }

            Set<Character> reachable;
            reachable = new HashSet<>();

            reachable.add(start);

            while (true) {
                boolean delta = false;
                for (char i = 0; i < edges.size(); i++) {
                    for (String to : edges.get(i)) {
                        if (reachable.contains(i)) {
                            for (int j = 0; j < to.length(); j++) {
                                if (Character.isUpperCase(to.charAt(j))) {
                                    int tmp = reachable.size();
                                    reachable.add(to.charAt(j));
                                    if (reachable.size() > tmp) {
                                        delta = true;
                                    }
                                }
                            }
                        }
                    }
                }
                if (!delta) {
                    break;
                }
            }

            for (char c = 0; c < 100; c++) {
                if (Character.isUpperCase(c) && was[c] && !reachable.contains(c)) {
                    unreachable.add(c);
                }
            }
        }

        private int parseSize(String l) {
            l = l.trim();
            int k = 0;
            StringBuilder sz = new StringBuilder();
            while (k < l.length() && Character.isDigit(l.charAt(k))) {
                sz.append(l.charAt(k));
                k++;
            }
            return Integer.parseInt(sz.toString());
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