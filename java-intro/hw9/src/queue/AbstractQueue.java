package queue;

abstract public class AbstractQueue implements Queue{
    protected int size = 0;

    public void enqueue(Object elem) {
        assert elem != null;

        enqueueImpl(elem);
        size++;
    }

    abstract protected void enqueueImpl(Object elem);

    public Object element() {
        assert size > 0;
        return elementImpl();
    }

    abstract protected Object elementImpl();

    public Object dequeue() {
        Object res = element();
        dequeueImpl();
        size--;
        return res;
    }

    abstract protected void dequeueImpl();

    public void clear() {
        clearImpl();
        size = 0;
    }

    abstract protected void clearImpl();

    public int size() {
        return size;
    }

    public boolean isEmpty() {
        return size == 0;
    }

    public Object[] toArray() {
        Object[] res = new Object[size()];
        for (int i = size; i < size(); i++) {
            res[i] = dequeue();
            enqueue(res[i]);
        }

        return res;
    }

}
