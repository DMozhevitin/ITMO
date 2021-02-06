package queue;

public class MyArrayQueueTest {
    public static void main(String[] args) {
        ArrayQueue q = new ArrayQueue();
        fill(q);
        test(q);
    }

    public static void fill(ArrayQueue q) {
        for (int i = 0; i < 5; i++) {
            q.enqueue(i);
        }

        for (int i = 5; i < 10; i++) {
            q.push(i);
        }
    }

    public static void test(ArrayQueue q) {
        while (!q.isEmpty()) {
            System.out.println(q.size() + " " + q.peek() + " " + q.element() + " " + q.remove() + " " + q.dequeue());
        }
    }
}
