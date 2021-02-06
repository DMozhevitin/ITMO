import java.io.*;
import java.util.*;

public class Task {
    public static void main(String[] args) throws IOException {
        InputReader scanner = new InputReader(System.in);
        OutputStream out = new BufferedOutputStream(System.out);
        Treap treap = new Treap();

        int n = Integer.parseInt(scanner.next());
        int m = Integer.parseInt(scanner.next());

        for (int i = 1; i <= m + n; i++) {
            treap.insert(i, 0);
        }

        for (int i = 1; i <= n; i++) {
            int x = Integer.parseInt(scanner.next());
            treap.query(x, i);
            treap.minIndex = Integer.MAX_VALUE;
        }

        treap.go(treap.root, 0);
        treap.getArray(treap.root);

        out.write(Integer.toString(treap.a.size()).getBytes());
        out.write("\n".getBytes());
        for (int item : treap.a) {
            out.write(Integer.toString(item).getBytes());
            out.write(" ".getBytes());
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
}

class Treap {
    Node root;
    Random randomGenerator = new Random();
    public static int minIndex = Integer.MAX_VALUE;
    public static int lastIndex = 0;

    public Treap() {
        root = null;
    }

    ArrayList<Integer> a = new ArrayList<>();

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

    private int vertexMin(Node vertex) {
        return vertex == null ? Integer.MAX_VALUE : vertex.min;
    }


    private void update(Node vertex) {
        if (vertex != null) {
            vertex.w = 1 + vertexWeight(vertex.left) + vertexWeight(vertex.right);
            vertex.sum = vertex.value + vertexSum(vertex.left) + vertexSum(vertex.right);
            vertex.min = Math.min(vertex.value, Math.min(vertexMin(vertex.left), vertexMin(vertex.right)));
        }
    }

    public void query(int index, int value) {
        Pair<Node, Node> splitted = split(root, index - 1);
        Node segment = splitted.value;
        find(segment, 0);
        segment = remove(segment, minIndex);
        segment = merge(new Node(value, randomGenerator.nextInt()), segment);
        root = merge(splitted.key, segment);
    }

    public void find(Node vertex, int index) {
        if (vertex == null) {
            return;
        }

        if (vertex.value == 0) {
            minIndex = Math.min(minIndex, index + vertexWeight(vertex.left) + 1);
        }

        if (vertexMin(vertex.left) == 0) {
            find(vertex.left, index);
        } else {
            find(vertex.right, index + vertexWeight(vertex.left) + 1);
        }
    }

    public void go(Node vertex, int index) {
        if (vertex == null) {
            return;
        }

        go(vertex.left, index);
        if (vertex.value != 0) {
            lastIndex = index + vertexWeight(vertex.left) + 1;
        }
        go(vertex.right, index + vertexWeight(vertex.left) + 1);
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

    public void getArray(Node vertex) {
        if (vertex == null || a.size() > lastIndex) {
            return;
        }

        getArray(vertex.left);
        if (a.size() < lastIndex) {
            a.add(vertex.value);
        }
        getArray(vertex.right);
    }

    public void print() {
        print(root);
    }

    private void print(Node vertex) {
        if (vertex == null) {
            return;
        }

        print(vertex.left);
        System.out.println(vertex.value + " " + vertex.w + " " + vertex.y);
        print(vertex.right);
    }
}

class Node {
    int value;
    int y;
    int w = 1;
    int min;
    int sum = 0;
    Node left = null;
    Node right = null;

    Node(int value, int y) {
        this.value = value;
        this.min = value;
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