package queue;

//Inv : (size >= 0) && (elems[i] != null ∀ i ∈ [0, size))
public class ArrayQueueADT {
    private int size = 0, head = 0, tail = 0, index = 0;
    private Object[] elems = new Object[10];

    //Pre : (elems.length != 0) && (x ∈ [0, elems.length))
    private static int inc(ArrayQueueADT q, int x) {
        return (x + 1) % q.elems.length;
    }
    //Post : x' = (x + 1) % elems.length

    //Pre : (elems.length != 0) && (x ∈ [0, elems.length))
    private static int dec(ArrayQueueADT q, int x) {
        return (x == 0 ? q.elems.length - 1 : x - 1);
    }
    //Post : (res = elems.length && x = 0) || (res = x - 1 && x > 0)

    //Pre : capacity >= 0
    private static void ensureCapacity(ArrayQueueADT q, int capacity) {
        if (capacity < q.elems.length && q.elems.length <= capacity * 4) {
            return;
        }

        Object[] newElems = new Object[2 * capacity];
        newElems = fillArray(q, newElems);

        q.head = 0;
        q.tail = q.index;
        q.elems = newElems;
    }
    //Post : (size' = size) && (elems' = elems) && (elems'.length ∈ (capacity, capacity * 4])

    //Pre : elem != null
    public static void enqueue(ArrayQueueADT q, Object elem) {
        assert elem != null;

        q.size++;
        ensureCapacity(q, q.size);
        q.elems[q.tail] = elem;
        q.tail = inc(q, q.tail);
    }
    //Post : (size' = size + 1) && (elems' = elems) && (elems'[size] = elem)

    //Pre : n > 0
    public static Object element(ArrayQueueADT q) {
        assert q.size > 0;

        return q.elems[q.head];
    }
    //Post : (size' = size) && (elems' = elems) && (res = elems[0])

    //Pre : n > 0
    public static Object dequeue(ArrayQueueADT q) {
        assert q.size > 0;

        Object res = element(q);
        q.head = inc(q, q.head);
        q.size--;
        ensureCapacity(q, q.size);
        return res;
    }
    //Post : (size' = size - 1) && (elems[i] = elems'[i - 1] ∀ i ∈ [1, size']) && (res = elems[0])

    //Pre : elem != null
    public static void push(ArrayQueueADT q, Object elem) {
        assert elem != null;

        q.size++;
        ensureCapacity(q, q.size);
        q.head = dec(q, q.head);
        q.elems[q.head] = elem;
    }
    //Post : (size' = size + 1) && (elems[i] = elems'[i + 1] ∀ i ∈ [0, size)) && (elem = elems'[0])

    //Pre : size > 0
    public static Object peek(ArrayQueueADT q) {
        assert q.size > 0;

        return q.elems[dec(q, q.tail)];
    }
    //Post : (size' = size) && (elems' = elems) && (res = elems[size - 1])


    //Pre : size > 0
    public static Object remove(ArrayQueueADT q) {
        assert q.size > 0;

        Object res = peek(q);
        q.tail = dec(q, q.tail);
        q.size--;
        ensureCapacity(q, q.size);

        return res;
    }
    //Post: (size' = size - 1) && (elems'[i] = elems[i] ∀ i ∈ [0, size')) && (res = elems[size'])


    //Pre : Skakov = zayka
    public static int size(ArrayQueueADT q) {
        return q.size;
    }
    //Post : (size' = size) && (elems' = elems) && (res = size)

    //Pre : Skakov = zayka
    public static boolean isEmpty(ArrayQueueADT q) {
        return q.size == 0;
    }
    //Post : (size' = size) && (elems' = elems) && (res = (size = 0))

    //Pre : Skakov = zayka
    public static void clear(ArrayQueueADT q) {
        q.size = q.head = q.tail = 0;
        q.elems = new Object[10];
    }
    //Post: size = 0

    //Pre : res != null && (res.empty())
    private static Object[] fillArray(ArrayQueueADT q, Object[] res) {
        assert res != null;

        q.index = 0;
        for (int i = q.head; i != q.tail; i = inc(q, i)) {
            res[q.index++] = q.elems[i];
        }
        return res;
    }
    //Post : res = elems

    //Pre : Skakov = zayka
    public static Object[] toArray(ArrayQueueADT q) {
        Object[] res = new Object[q.size];
        res = fillArray(q, res);
        return res;
    }
    //Post : res = elems
}
