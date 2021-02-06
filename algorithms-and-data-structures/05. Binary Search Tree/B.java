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

        while (scanner.hasNext()) {
            String query = scanner.next();
            int x = Integer.parseInt(scanner.next());
            switch (query) {
                case "insert":
                    treap.insert(x);
                    break;

                case "delete":
                    treap.remove(x);
                    break;

                case "exists":
                    if (treap.isEmpty()) {
                        System.out.println("false");
                    } else {
                        System.out.println(treap.contains(x));
                    }
                    break;

                case "next":
                    if (treap.isEmpty()) {
                        System.out.println("none");
                    } else {
                        if (treap.next(x) == null) {
                            System.out.println("none");
                        } else {
                            System.out.println(treap.next(x));
                        }
                    }
                    break;

                case "prev":
                    if (treap.isEmpty()) {
                        System.out.println("none");
                    } else {
                        if (treap.prev(x) == null) {
                            System.out.println("none");
                        } else {
                            System.out.println(treap.prev(x));
                        }
                    }
                    break;
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

    private Pair<Node, Node> split(Node vertex, int key) {
        if (vertex == null) {
            return new Pair<>(null, null);
        }

        if (key > vertex.x) {
            Pair<Node, Node> splitted = split(vertex.right, key);
            vertex.right = splitted.key;
            return new Pair<>(vertex, splitted.value);
        } else {
            Pair<Node, Node> splitted = split(vertex.left, key);
            vertex.left = splitted.value;
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
            return t1;
        } else {
            t2.left = merge(t1, t2.left);
            return t2;
        }
    }

    public void insert(int x) {
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


    public void remove(int x) {
        if (root != null) {
            root = remove(root, x);
        }
    }

    private Node remove(Node vertex, int x) {
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

    public Integer search(int x) {
        Node found = search(root, x);
        if (found == null) {
            return null;
        } else {
            return found.x;
        }
    }

    private Node search(Node vertex, int x) {
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

    public boolean contains(int x) {
        return contains(root, x);
    }

    private boolean contains(Node vertex, int x) {
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

    public Integer findMin() {
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

    public Integer next(int x) {
        Node found = next(root, x);
        if (found == null) {
            return null;
        } else {
            return found.x;
        }
    }

    private Node next(Node vertex, int x) {
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

    public Integer prev(int x) {
        Node found = prev(root, x);
        if (found == null) {
            return null;
        } else {
            return found.x;
        }
    }

    private Node prev(Node vertex, int x) {
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

    public Integer findMax() {
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
    int x, y;
    Node left = null;
    Node right = null;

    Node(int x, int y) {
        this.x = x;
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