import java.io.*;
import java.util.*;

public class Task {
    public static void main(String[] args) throws IOException {
        InputReader reader = new InputReader(System.in);
        BufferedOutputStream out = new BufferedOutputStream(System.out);
        int n = reader.nextInt();

        Tree tree = new Tree(n, reader);

        int m = reader.nextInt();
        Integer[][] groups = new Integer[m + 1][];

        for (int i = 0; i < m; i++) {
            int groupSize = reader.nextInt();

            groups[i] = new Integer[groupSize];

            for (int j = 0; j < groupSize; j++) {
                groups[i][j] = reader.nextInt();
            }
        }

        for (int i = 0; i < m; i++) {
            out.write(Integer.toString(tree.groupAntiquity(groups[i])).getBytes());
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
        int[] tIn;
        int root;
        int log;
        int time;
        ArrayList<ArrayList<Integer>> edges;

        Tree(int size, InputReader source) {
            this.size = size;
            this.parent = new int[size + 10];
            this.depth = new int[size + 10];
            this.tIn = new int[size + 10];
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

        public int groupAntiquity(Integer[] group) {
            Arrays.sort(group, new CompByTIn());
            if (group.length == 1) {
                return depth[group[0]] + 1;
            } else {
                int antiquity = depth[group[0]];
                int lca = group[0];
                for (int i = 1; i < group.length; i++) {
                    lca = lca(lca, group[i]);
                    antiquity += depth[group[i]] - depth[lca];
                    lca = group[i];
                }

                return antiquity + 1;
            }
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
            tIn[v] = time;
            time++;
            depth[v] = d;
            tIn[v] = time;
            for (int i = 0; i < edges.get(v).size(); i++) {
                int to = edges.get(v).get(i);
                dfs(to, d + 1);
            }
        }

        private void getParents(InputReader source) {

            for (int i = 1; i <= size; i++) {
                int tmp = source.nextInt();
                parent[i] = tmp;
                if (tmp == -1) {
                    root = i;
                    parent[i] = 0;
                } else {
                    edges.get(tmp).add(i);
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
            for (int i = 1; i <= size; i++) {
                System.out.print(i + ": ");
//                for (int j = 0; j < edges.get(i).size(); j++) {
//                    System.out.print(edges.get(i).get(j) + " ");
//                }
                System.out.println(depth[i]);
            }
        }

        class CompByTIn implements Comparator{
            public int compare(Object o1, Object o2) {
                int a = (Integer)o1;
                int b = (Integer)o2;

                if (tIn[a] > tIn[b]) {
                    return -1;
                } else if(tIn[b] == tIn[a]) {
                    return 0;
                } else {
                    return 1;
                }
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