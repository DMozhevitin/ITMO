    import java.io.*;
    import java.util.*;

    public class Task {
        public static void main(String[] args) {
            Scanner scanner = new Scanner(System.in);
            Treap treap = new Treap();

            int n = Integer.parseInt(scanner.next());
            int m = Integer.parseInt(scanner.next());

            for (int i = 1; i <= n; i++) {
                treap.insert(i, i);
            }

            for (int i = 0; i < m; i++) {
                int left = Integer.parseInt(scanner.next());
                int right = Integer.parseInt(scanner.next());
                treap.toBegin(left, right);
//                treap.print();
            }
            treap.print();
            scanner.close();
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

    class Treap {
        Node root;
        Random randomGenerator = new Random();

        public Treap() {
            root = null;
        }

        public boolean isEmpty() {
            return root == null;
        }

        public int size() {
            return root.w;
        }

        private Pair<Node, Node> split(Node vertex, int k) {
            if (vertex == null) {
                return new Pair<>(null, null);
            }

            int w = vertexWeight(vertex.left);
            if (w < k) {
                Pair<Node, Node> splitted = split(vertex.right, k - w - 1);
                vertex.right = splitted.key;
                update(vertex);
                return new Pair<>(vertex, splitted.value);
            } else {
                Pair<Node, Node> splitted = split(vertex.left, k);
                vertex.left = splitted.value;
                update(vertex);
                return new Pair<>(splitted.key, vertex);
            }
        }


        public void test() {
                Pair<Node, Node> splitted = split(root, 4);

            print(splitted.key);
            System.out.println("--------");
            print(splitted.value);
        }

        private Node merge(Node t1, Node t2) {
            if (t1 == null) {
                return t2;
            }

            if (t2 == null) {
                return t1;
            }

            if (t1.y > t2.y) {
                t1.right = merge(t1.right, t2);
                update(t1);
                return t1;
            } else {
                t2.left = merge(t1, t2.left);
                update(t2);
                return t2;
            }
        }

        private int vertexWeight(Node vertex) {
            if (vertex == null) {
                return 0;
            } else {
                return vertex.w;
            }
        }

        private int vertexSum(Node vertex) {
            if (vertex == null) {
                return 0;
            } else {
                return vertex.sum;
            }
        }

        public void insert(int pos, int value) {
            if (root == null) {
                root = new Node(value, randomGenerator.nextInt());
            } else {
                root = insert(root, pos, value);
            }
        }

        private void update(Node vertex) {
            if (vertex != null) {
                int weight = 0;
                int sum = 0;
                if (vertex.left != null) {
                    weight += vertex.left.w;
                    sum += vertex.left.sum;
                }
                if (vertex.right != null) {
                    weight += vertex.right.w;
                    sum += vertex.right.sum;
                }

                vertex.w = weight + 1;
                vertex.sum = sum + vertex.value;
            }
        }

        private Node insert(Node vertex, int pos, int value) {
            Pair<Node, Node> splitted = split(vertex, pos - 1);
            splitted.key = merge(splitted.key, new Node(value, randomGenerator.nextInt()));
            vertex = merge(splitted.key, splitted.value);
            return vertex;
        }

        public void remove(int pos) {
            if (root != null) {
                root = remove(root, pos);
            }
        }

        private Node remove(Node vertex, int pos) {
            Pair<Node, Node> splitted = split(vertex, pos - 1);
            Pair<Node, Node> splitted2 = split(splitted.value, 1);

            vertex = merge(splitted.key, splitted2.value);
            update(vertex);
            return vertex;
        }

        public void toBegin(int left, int right) {
            Pair<Node, Node> splitted = split(root, left - 1);
            Pair<Node, Node> splitted2 = split(splitted.value, right - left + 1);
            root = merge(splitted2.key, splitted.key);
            root = merge(root, splitted2.value);
        }

        public int sum(int left, int right) {
            Pair<Node, Node> splitted = split(root, left - 1);
            Pair<Node, Node> splitted2 = split(splitted.value, right - left + 1);
            int ans = vertexSum(splitted2.key);
            root = merge(splitted.key, splitted.value);
            root = merge(root, splitted2.value);
            return ans;
        }

        public void print() {
            print(root);
        }

        private void print(Node vertex) {
            if (vertex == null) {
                return;
            }

            print(vertex.left);
            System.out.print(vertex.value + " ");
            print(vertex.right);
        }
    }

    class Node {
        int value, y, w = 1;
        int sum = 0;
        Node left = null;
        Node right = null;

        Node(int value, int y) {
            this.value = value;
            this.y = y;
        }
    }

    final class Pair<K, V> implements Map.Entry<K, V> {
        public K key;
        public V value;

        public Pair(K key, V value) {
            this.key = key;
            this.value = value;
        }

        public K getKey() {
            return key;
        }

        public V getValue() {
            return value;
        }

        public V setValue(V value) {
            V old = this.value;
            this.value = value;
            return old;
        }
    }