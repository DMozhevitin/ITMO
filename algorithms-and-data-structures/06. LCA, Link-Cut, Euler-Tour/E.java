
import java.io.*;
import java.util.*;

public class Task {
    public static void main(String[] args) throws IOException {
        InputReader reader = new InputReader(System.in);
        BufferedOutputStream out = new BufferedOutputStream(System.out);
        int n = reader.nextInt();
        Tree tree = new Tree(n, reader);

        int m = reader.nextInt();


        for (int i = 0; i < m; i++) {
            int x = reader.nextInt();
            int y = reader.nextInt();
            tree.query(x, y);
        }

        System.out.println(tree.countUselessEdges());
//        tree.print();
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

    static class Tree {
        int size;
        int[] parent;
        int[] depth;
        int[][] dp;
        int root = 1;
        int log;
        int[] maxJump;
        int[] markedLength;
        int[] ost;
        Pair[] m;
        ArrayList<Set<Integer>> usefulEdges;
        Set<Map.Entry<Integer, Integer>> useful;
        ArrayList<Pair> mxJump;
        ArrayList<ArrayList<Integer>> edges;

        Tree(int size, InputReader source) {
            ost = new int[size + 10];
            mxJump = new ArrayList<>();
            for (int i = 0; i <= size + 10; i++) {
                mxJump.add(new Pair(i, -Integer.MAX_VALUE));
            }
            markedLength = new int[size + 10];
            maxJump = new int[size + 10];
            useful = new HashSet<>();
            this.size = size;
            usefulEdges = new ArrayList<>();
            this.parent = new int[size + 10];
            this.depth = new int[size + 10];
            this.dp = new int[size + 10][log2(size) + 1];
            this.edges = new ArrayList<>();
            for (int i = 0; i <= size + 10; i++) {
                edges.add(new ArrayList<>());
                usefulEdges.add(new HashSet<>());
            }
            log = log2(size);

            readTree(source);
            setParents();
            setDp();
            dfs(root, 1,0);
        }

        public int countUselessEdges() {
            return size - 1 - countPaths(1, 0);
        }

        public void query(int v, int u) {
            int lca = lca(v, u);
            getMaxJump(v, lca);
            getMaxJump(u, lca);
        }

        private void getMaxJump(int v, int p) {
            mxJump.set(v, (new Pair(v, Math.max(Math.abs(depth[v] - depth[p]), mxJump.get(v).value))));
        }


        public int lca(int v, int u) {
            if (depth[v] > depth[u]) {
                int tmp = v;
                v = u;
                u = tmp;
            }

            int h = depth[u] - depth[v];
            for (int i = log; i >= 0; i--) {
                if ((1 << i) <= h) {
                    h -= (1 << i);
                    u = dp[u][i];
                }
            }

            if (u == v) {
                return v;
            }

            for (int i = log; i >= 0; i--) {
                if (dp[v][i] != dp[u][i]) {
                    v = dp[v][i];
                    u = dp[u][i];
                }
            }

            return parent[v];
        }

        private int countPaths(int v, int p) {
            int cnt = 0;
            for (Integer to : edges.get(v)) {
                if (to == p) {
                    continue;
                }
                cnt += countPaths(to, v);
                mxJump.set(v, new Pair(v, Math.max(mxJump.get(v).value, mxJump.get(to).value - 1)));
            }

            if (mxJump.get(v).value > 0) {
                return ++cnt;
            } else {
                return cnt;
            }
        }

        private void dfs(int v, int prev, int d) {
            depth[v] = d;
            for (int i = 0; i < edges.get(v).size(); i++) {
                int to = edges.get(v).get(i);
                if (to != prev) {
                    dfs(to, v, d + 1);
                }
            }
        }

        private void readTree(InputReader source) {
            for (int i = 0; i < size - 1; i++) {
                int x = source.nextInt();
                int y = source.nextInt();
                edges.get(x).add(y);
                edges.get(y).add(x);
            }
        }

        private void setParents() {
            parent[1] = 0;
            dfsP(1, 0);
        }

        private void dfsP(int v, int prev) {
            for (int i = 0; i < edges.get(v).size(); i++) {
                int to = edges.get(v).get(i);
                if (to != prev) {
                    parent[to] = v;
                    dfsP(to, v);
                }
            }
        }

        private void setDp() {
            for (int i = 1; i <= size; i++) {
                dp[i][0] = parent[i];
            }

            for (int j = 1; j <= log; j++) {
                for (int i = 1; i <= size; i++) {
                    dp[i][j] = dp[dp[i][j - 1]][j - 1];
                }
            }
        }

        public void print() {
//            for (int i = 1; i <= size; i++) {
//                System.out.print(i + ": ");
//                for (int j = 0; j < edges.get(i).size(); j++) {
//                    System.out.print(edges.get(i).get(j) + " ");
//                }
//                System.out.println();
//            }
//
//            for (int i = 1; i <= size; i++) {
//                System.out.println(parent[i]);
//            }
            for (int i = 1; i <= size; i++) {
                System.out.println(i + ": " + ost[i]);
            }
        }
    }

    public static int log2(int n) {
        if (n == 0) {
            return 0;
        }

        return log2(n / 2) + 1;
    }

}

final class Pair implements Comparable{
    public int key;
    public int value;

    public Pair(int key, int value) {
        this.key = key;
        this.value = value;
    }

    @Override
    public int compareTo(Object o) {
        Pair p2 = (Pair)o;
        if (this.key == p2.key) {
            if (this.value < p2.value) {
                return 1;
            } else if (this.value == p2.value) {
                return 0;
            } else {
                return -1;
            }
        } else {
            if (this.key < p2.key) {
                return 1;
            } else if (this.key == p2.key) {
                return 0;
            } else {
                return -1;
            }
        }
    }
}