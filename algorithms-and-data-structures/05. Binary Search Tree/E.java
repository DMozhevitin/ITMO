import java.io.*;
import java.util.Map;
import java.util.Random;
import java.util.Scanner;
import java.util.StringTokenizer;

public class Task {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        PrintWriter writer = new PrintWriter(System.out);
        Treap treap = new Treap();

        int n = Integer.parseInt(scanner.next());

        long y = 0;
        boolean prevQueue = false;

        for (int i = 0; i < n; i++) {
            String query = scanner.next();

            if (query.equals("+")) {
                long x = Long.parseLong(scanner.next());
                if (prevQueue) {
                    treap.insert((x % (long)1e9 + (long)y % (long)1e9) % (long) 1e9);
                } else {
                    treap.insert(x);
                }
                prevQueue = false;
            } else {
                long left = Long.parseLong(scanner.next());
                long right = Long.parseLong(scanner.next());

                long ans = treap.querySum(left, right);
                y = ans;
                prevQueue = true;
                System.out.println(ans);
            }
        }
        scanner.close();
        writer.close();
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

    private Pair<Node, Node> split(Node vertex, long key) {
        if (vertex == null) {
            return new Pair<>(null, null);
        }

        if (key > vertex.x) {
            Pair<Node, Node> splitted = split(vertex.right, key);
            vertex.right = splitted.key;
            update(vertex);
            return new Pair<>(vertex, splitted.value);
        } else {
            Pair<Node, Node> splitted = split(vertex.left, key);
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

    private long vertexSum(Node vertex) {
        return vertex == null ? 0 : vertex.sum;
    }

    private void update(Node vertex) {
        if (vertex != null) {
            vertex.sum = vertex.x + vertexSum(vertex.left) + vertexSum(vertex.right);
        }
    }

    public void insert(long x) {
        if (root == null) {
            root = new Node(x, randomGenerator.nextInt());
        } else {
            if (search(x) == null || search(x) != x) {
                root = insert(root, new Node(x, randomGenerator.nextInt()));
            }
        }
    }

    private Node insert(Node vertex, Node k) {
        Pair<Node, Node> splitted = split(vertex, k.x);
        splitted.key = merge(splitted.key, k);
        vertex = merge(splitted.key, splitted.value);
        return vertex;
    }


    public void remove(long x) {
        if (root != null) {
            root = remove(root, x);
        }
    }

    private Node remove(Node vertex, long x) {
        if (vertex == null) {
            return null;
        }

        if (vertex.x == x) {
            return merge(vertex.left, vertex.right);
        }

        if (x < vertex.x) {
            vertex.left = remove(vertex.left, x);
        } else {
            vertex.right = remove(vertex.right, x);
        }

        return vertex;
    }

    public long querySum(long left, long right) {
        long l = getSum(root, left, 0);
        long r = getSum(root, right + 1, 0);
        return r - l;
    }

    private long getSum(Node vertex, long key, long sum) {
        if (vertex == null) {
            return sum;
        }

        if (key > vertex.x) {
            return getSum(vertex.right, key, sum + vertexSum(vertex.left) + vertex.x);
        } else {
            return getSum(vertex.left, key, sum);
        }
    }

    public Long search(long x) {
        Node found = search(root, x);
        if (found == null) {
            return null;
        } else {
            return found.x;
        }
    }

    private Node search(Node vertex, long x) {
        if (vertex == null) {
            return null;
        }

        if (vertex.x == x) {
            return vertex;
        }

        if (x < vertex.x) {
            if (vertex.left == null) {
                return vertex;
            }
            return search(vertex.left, x);
        } else {
            if (vertex.right == null) {
                return vertex;
            }
            return search(vertex.right, x);
        }
    }

    public boolean contains(long x) {
        return contains(root, x);
    }

    private boolean contains(Node vertex, long x) {
        if (x == vertex.x) {
            return true;
        }

        if (x < vertex.x) {
            if (vertex.left == null) {
                return false;
            }

            return contains(vertex.left, x);
        } else {
            if (vertex.right == null) {
                return false;
            }

            return contains(vertex.right, x);
        }
    }

    public Long findMin() {
        Node found = findMin(root);
        if (found == null) {
            return null;
        } else {
            return found.x;
        }
    }

    private Node findMin(Node vertex) {
        if (vertex.left == null) {
            return vertex;
        }

        return findMin(vertex.left);
    }

    public Long next(long x) {
        Node found = next(root, x);
        if (found == null) {
            return null;
        } else {
            return found.x;
        }
    }

    private Node next(Node vertex, long x) {
        Node current = vertex;
        Node successor = null;

        while (current != null) {
            if (x < current.x) {
                successor = current;
                current = current.left;
            } else {
                current = current.right;
            }
        }

        return successor;
    }

    public Long prev(long x) {
        Node found = prev(root, x);
        if (found == null) {
            return null;
        } else {
            return found.x;
        }
    }

    private Node prev(Node vertex, long x) {
        Node current = vertex;
        Node successor = null;

        while (current != null) {
            if (x <= current.x) {
                current = current.left;
            } else {
                successor = current;
                current = current.right;
            }
        }

        return successor;
    }

    public Long findMax() {
        Node found = findMax(root);
        if (found == null) {
            return null;
        } else {
            return found.x;
        }
    }

    private Node findMax(Node vertex) {
        if (vertex.right == null) {
            return vertex;
        }

        return findMax(vertex.right);
    }

    public void print() {
        print(root);
    }

    private void print(Node vertex) {
        if (vertex == null) {
            return;
        }

        print(vertex.left);
        System.out.println(vertex.x + " " + vertex.y);
        print(vertex.right);
    }
}

class Node {
    int y;
    long x, sum;
    Node left = null;
    Node right = null;

    Node(long x, int y) {
        this.x = x;
        this.y = y;
        this.sum = x;
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