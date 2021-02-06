package queue;

public class LinkedQueue extends AbstractQueue {
    private class Node {
        private Node next;
        private Object val;

        private Node(Object value, Node next) {
            next = next;
            val = val;
        }
    }

    private Node head, tail;

    protected void enqueueImpl(Object elem) {
        if (size == 0) {
            head = tail = new Node(elem, null);
        } else {
            tail.next = new Node(elem, null);
            tail = tail.next;
        }
    }

    protected void dequeueImpl() {
        head = head.next;
        if (size == 1) {
            tail = null;
        }
    }

    protected Object elementImpl() {
        return head.val;
    }

    protected void clearImpl() {
        head = tail = null;
    }
}
