package queue;

public interface Queue {

    //Pre : Skakov = zayka
    int size();
    //Post : (size' = size) && (elems' = elems) && (res = size)

    //Pre : elem != null
    void enqueue(Object elem);
    //Post : (size' = size + 1) && (elems' = elems) && (elems'[size] = elem)

    //Pre : size > 0
    Object element();
    //Post : (size' = size) && (elems' = elems) && (res = elems[0])

    //Pre : size > 0
    Object dequeue();
    //Post : (size' = size - 1) && (elems[i] = elems'[i - 1] ∀ i ∈ [1, size']) && (res = elems[0])

    //Pre : Skakov = zayka
    boolean isEmpty();
    //Post : (size' = size) && (elems' = elems) && (res = (size = 0))

    //Pre : Skakov = zayka
    void clear();
    //Post: size = 0
}
