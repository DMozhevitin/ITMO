package queue;

public class ArrayQueue extends AbstractQueue {
    private int head = 0, tail = 0, index = 0;
    private Object[] elems = new Object[10];

    public ArrayQueue(int s) {
        elems = new Object[s];
    }

    public ArrayQueue() {
        elems = new Object[10];
    }

    private int inc(int x) {
        return (x + 1) % elems.length;
    }

    private int dec(int x) {
        return (x == 0 ? elems.length - 1 : x - 1);
    }

    private void ensureCapacity(int capacity) {
        if (capacity < elems.length && elems.length <= capacity * 4) {
            return;
        }

        Object[] newElems = new Object[2 * capacity];
        newElems = fillArray(newElems);

        head = 0;
        tail = index;
        elems = newElems;

    }

    private Object[] fillArray(Object[] res) {
        assert res != null;

        index = 0;
        for (int i = head; i != tail; i = inc(i)) {
            res[index++] = elems[i];
        }
        return res;
    }

    protected void enqueueImpl(Object elem) {
        ensureCapacity(size + 1);
        elems[tail] = elem;
        tail = inc(tail);
    }

    protected Object elementImpl() {
        return elems[head];
    }

    protected void dequeueImpl() {
        //assert size > 0;

        elems[head] = null;
        head = inc(head);
        ensureCapacity(size - 1);
    }

    protected void clearImpl() {
        head = tail = 0;
        ensureCapacity(1);
    }
}
