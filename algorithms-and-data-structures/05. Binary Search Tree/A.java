import java.io.*;
import java.util.Scanner;
import java.util.StringTokenizer;

public class Task {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        PrintWriter writer = new PrintWriter(System.out);

        Node root = null;
        boolean b = false;
        while (scanner.hasNext()) {
            String query = scanner.next();
            int x = Integer.parseInt(scanner.next());
            Node found;
            switch (query) {
                case "insert":
                    if (root == null) {
                        root = new Node(x, null);
                    } else {
                        root.insert(root, x);
                    }
                    break;

                case "delete":
                    if (root != null) {
                        root = root.delete(root, x);
                    }
                    break;

                case "exists":
                    if (root != null) {
                        System.out.println(root.contains(root, x));
                    } else {
                        System.out.println("false");
                    }
                    break;

                case "next":
                    if (root == null) {
                        System.out.println("none");
                    } else {
                        found = root.search(root, x);
//                    if (found != null) System.out.println("found = " + found.key);
                        if (found == null) {
                            System.out.println("none");
                        } else {
                            if (found.key > x) {
                                System.out.println(found.key);
                            } else {
                                found = root.next(found);
                                if (found == null) {
                                    System.out.println("none");
                                } else {
                                    System.out.println(found.key);
                                }
                            }
                        }
                    }
                    break;

                case "prev":
                    if (root == null) {
                        System.out.println("none");
                    } else {
                        found = root.search(root, x);
//                    if (found != null) System.out.println("found = " + found.key);
                        if (found == null) {
                            System.out.println("none");
                        } else {
                            if (found.key < x) {
                                System.out.println(found.key);
                            } else {
                                found = root.prev(found);
                                if (found == null) {
                                    System.out.println("none");
                                } else {
                                    System.out.println(found.key);
                                }
                            }
                        }
                    }
                    break;

                default:

                    break;
            }
        }
        scanner.close();
        writer.close();
    }

    static class Node {
        int key;
        Node left = null;
        Node right = null;
        Node parent = null;

        Node(int key, Node parent) {
            this.key = key;
            this.parent = parent;
        }

        void insert(Node vertex, int x) {
            if (vertex.key == x) {
                return;
            }

            if (x < vertex.key) {
                if (vertex.left == null) {
                    vertex.left = new Node(x, vertex);
                } else {
                    insert(vertex.left, x);
                }
            } else {
                if (vertex.right == null) {
                    vertex.right = new Node(x, vertex);
                } else {
                    insert(vertex.right, x);
                }
            }
        }

        Node search(Node vertex, int x) {
            if (vertex == null) {
                return vertex;
            }

            if (vertex.key == x) {
                return vertex;
            }

            if (x < vertex.key) {
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

        boolean contains(Node vertex, int x) {
            if (x == vertex.key) {
                return true;
            }

            if (x < vertex.key) {
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

        Node findMin(Node vertex) {
            if (vertex.left == null) {
                return vertex;
            }

            return findMin(vertex.left);
        }

        Node findMax(Node vertex) {
            if (vertex.right == null) {
                return vertex;
            }

            return findMax(vertex.right);
        }

        Node delete(Node vertex, int x) {
            if (vertex == null) {
                return vertex;
            }

            if (x < vertex.key) {
                vertex.left = delete(vertex.left, x);
            } else if (x > vertex.key) {
                vertex.right = delete(vertex.right, x);
            } else if (vertex.left != null && vertex.right != null) {
                vertex.key = findMin(vertex.right).key;
                vertex.right = delete(vertex.right, vertex.key);
            } else {
                if (vertex.left != null) {
                    Node tmp = vertex;
                    vertex = vertex.left;
                    vertex.parent = tmp.parent;
                } else {
                    Node tmp = vertex;
                    vertex = vertex.right;
                    if (vertex != null) {
                        vertex.parent = tmp.parent;
                    }
                }
            }

            return vertex;
        }

        Node next(Node vertex) {
            if (vertex.right != null) {
                return findMin(vertex.right);
            }

            Node p = vertex.parent;
            while (p != null && vertex == p.right) {
                vertex = p;
                p = vertex.parent;
            }

            return p;
        }

        Node prev(Node vertex) {
            if (vertex.left != null) {
                return findMax(vertex.left);
            }

            Node p = vertex.parent;
            while (p != null && vertex == p.left) {
                vertex = p;
                p = vertex.parent;
            }

            return p;
        }

        void print(Node vertex) {
            if (vertex == null) {
                return;
            }

            print(vertex.left);
            System.out.println(vertex.key + " " + (vertex.parent == null ? "null" : vertex.parent.key));
            print(vertex.right);
        }
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