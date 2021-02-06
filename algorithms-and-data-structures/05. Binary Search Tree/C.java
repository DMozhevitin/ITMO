import java.io.*;
import java.util.*;

public class Task {
    public static void main(String[] args) throws IOException {
        InputReader reader = new InputReader(System.in);

        int n = Integer.parseInt(reader.next());
        ArrayCarrier.res = new Node[n];
        Node[] a = new Node[n];

        for (int i = 0; i < n; i++) {
            int x = Integer.parseInt(reader.next());
            int y = Integer.parseInt(reader.next());

            Node tmp = new Node(x, y, i + 1, null);
            a[i] = tmp;
        }

        Arrays.sort(a, new CompByX());
        Node root = a[0];
        Node prev = root;

        for (int i = 1; i < a.length; i++) {
            Node current = a[i];
            while (prev != root && current.y < prev.y) {
                prev = prev.parent;
            }

            if (prev == root && current.y < prev.y) {
                Node tmp = root;

                root = current;
                prev = root;
                root.left = tmp;
                tmp.parent = root;
            } else {
                Node tmp = prev.right;
                prev.right = current;
                current.parent = prev;
                prev = current;
                prev.left = tmp;
                if (tmp != null) {
                    tmp.parent = prev;
                }
            }
        }

        System.out.println("YES");
        root.getArray(root);

        OutputStream out = new BufferedOutputStream(System.out);
        for (Node vertex : ArrayCarrier.res) {
            if (vertex.parent == null) {
                out.write("0 ".getBytes());
            } else {
                out.write(Integer.toString(vertex.parent.index).getBytes());
                out.write(" ".getBytes());
            }

            if (vertex.left == null) {
                out.write("0 ".getBytes());
            } else {
                out.write(Integer.toString(vertex.left.index).getBytes());
                out.write(" ".getBytes());
            }

            if (vertex.right == null) {
                out.write("0 ".getBytes());
            } else {
                out.write(Integer.toString(vertex.right.index).getBytes());
                out.write(" ".getBytes());
            }

            out.write("\n".getBytes());
        }

        out.close();
    }

    static class CompByX implements Comparator<Node> {
        public int compare(Node obj1, Node obj2) {
            return Integer.compare(obj1.x, obj2.x);
        }
    }

    static class Node {
        int x;
        int y;
        int index;
        Node left = null;
        Node right = null;
        Node parent = null;
        Node(int x, int y, int index, Node parent) {
            this.x = x;
            this.y = y;
            this.index = index;
            this.parent = parent;
        }

        void getArray(Node vertex) {
            if (vertex == null) {
                return;
            }

            getArray(vertex.left);
            ArrayCarrier.res[vertex.index - 1] = vertex;
            getArray(vertex.right);
        }

    }

    static class ArrayCarrier {
        public static Node[] res;
    }

}

class InputReader {
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
}