package queue;

import java.util.ArrayList;

//Inv : (size >= 0) && (elems[i] != null ∀ i ∈ [0, size))
public class ArrayQueueModule {
    private static int size = 0, head = 0, tail = 0, index = 0;
    private static Object[] elems = new Object[10];

    //Pre : (elems.length != 0) && (x ∈ [0, elems.length))
    private static int inc(int x) {
        return (x + 1) % elems.length;
    }
    //Post : x' = (x + 1) % elems.length

    //Pre : (elems.length != 0) && (x ∈ [0, elems.length))
    private static int dec(int x) {
        return (x == 0 ? elems.length - 1 : x - 1);
    }
    //Post : (res = elems.length && x = 0) || (res = x - 1 && x > 0)

    //Pre : capacity >= 0
    private static void ensureCapacity(int capacity) {
        if (capacity < elems.length && elems.length <= capacity * 4) {
            return;
        }

        Object[] newElems = new Object[2 * capacity];
        index = 0;
        newElems = fillArray(newElems);

        head = 0;
        tail = index;
        elems = newElems;
    }
    //Post : (size' = size) && (elems' = elems) && (elems'.length ∈ (capacity, capacity * 4])

    //Pre : elem != null
    public static void enqueue(Object elem) {
        assert elem != null;

        size++;
        ensureCapacity(size);
        elems[tail] = elem;
        tail = inc(tail);
    }
    //Post : (size' = size + 1) && (elems' = elems) && (elems'[size] = elem)

    //Pre : n > 0
    public static Object element() {
        assert size > 0;

        return elems[head];
    }
    //Post : (size' = size) && (elems' = elems) && (res = elems[0])

    //Pre : n > 0
    public static Object dequeue() {
        assert size > 0;

        Object res = element();
        head = inc(head);
        size--;
        ensureCapacity(size);
        return res;
    }
    //Post : (size' = size - 1) && (elems[i] = elems'[i - 1] ∀ i ∈ [1, size']) && (res = elems[0])

    //Pre : elem != null
    public static void push(Object elem) {
        assert elem != null;

        size++;
        ensureCapacity(size);
        head = dec(head);
        elems[head] = elem;
    }
    //Post : (size' = size + 1) && (elems[i] = elems'[i + 1] ∀ i ∈ [0, size)) && (elem = elems'[0])

    //Pre : size > 0
    public static Object peek() {
        assert size > 0;

        return elems[dec(tail)];
    }
    //Post : (size' = size) && (elems' = elems) && (res = elems[size - 1])


    //Pre : size > 0
    public static Object remove() {
        assert size > 0;

        Object res = peek();
        tail = dec(tail);
        size--;
        ensureCapacity(size);

        return res;
    }
    //Post: (size' = size - 1) && (elems'[i] = elems[i] ∀ i ∈ [0, size')) && (res = elems[size'])


    //Pre : Skakov = zayka
    public static int size() {
        return size;
    }
    //Post : (size' = size) && (elems' = elems) && (res = size)

    //Pre : Skakov = zayka
    public static boolean isEmpty() {
        return size == 0;
    }
    //Post : (size' = size) && (elems' = elems) && (res = (size = 0))

    //Pre : Skakov = zayka
    public static void clear() {
        size = head = tail = 0;
        elems = new Object[10];
    }
    //Post: size = 0

    //Pre : res != null && (res.empty())
    private static Object[] fillArray(Object[] res) {
        assert res != null;

        index = 0;
        for (int i = head; i != tail; i = inc(i)) {
            res[index++] = elems[i];
        }
        return res;
    }
    //Post : res = elems

    //Pre : Skakov = zayka
    public static Object[] toArray() {
        Object[] res = new Object[size];
        res = fillArray(res);
        return res;
    }
    //Post : res = elems
}
