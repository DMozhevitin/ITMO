import java.io.*;
import java.util.*;

public class Task {
    public static void main(String[] args) throws IOException {
        InputReader reader = new InputReader(System.in);
        BufferedOutputStream out = new BufferedOutputStream(System.out);
        int n = reader.nextInt();
        int[] parent = new int[n + 1];
        int[] depth = new int[n + 1];
        int[][] dp = new int[n + 1][50];

        Tree tree = new Tree(n, reader);

        int m = reader.nextInt();

        for (int i = 0; i < m; i++) {
            int v = reader.nextInt();
            int u = reader.nextInt();

            out.write(Integer.toString(tree.lca(v, u)).getBytes());
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

    static class Tree {
        int size;
        int[] parent;
        int[] depth;
        int[][] dp;
        int root = 1;
        int log;
        ArrayList<ArrayList<Integer>> edges;

        Tree(int size, InputReader source) {
            this.size = size;
            this.parent = new int[size + 10];
            this.depth = new int[size + 10];
            this.dp = new int[size + 10][log2(size) + 1];
            this.edges = new ArrayList<>();
            for (int i = 0; i <= size; i++) {
                edges.add(new ArrayList<>());
            }
            log = log2(size);

            getParents(source);
            setDp();
            dfs(root, 0);
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

        private void dfs(int v, int d) {
            depth[v] = d;
            for (int i = 0; i < edges.get(v).size(); i++) {
                int to = edges.get(v).get(i);
                dfs(to, d + 1);
            }
        }

        private void getParents(InputReader source) {

            for (int i = 2; i <= size; i++) {
                int tmp = source.nextInt();
                parent[i] = tmp;
                edges.get(tmp).add(i);
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
            for (int i = 1; i <= size; i++) {
                System.out.print(i + ": ");
                for (int j = 0; j < edges.get(i).size(); j++) {
                    System.out.print(edges.get(i).get(j) + " ");
                }
                System.out.println();
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