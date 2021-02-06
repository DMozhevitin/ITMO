package ru.ifmo.rain.mozhevitin.arrayset;

import java.util.*;

public class ArraySet<T> extends AbstractSet<T> implements NavigableSet<T> {
    private final ReversedArrayList<T> arrayList;
    private final Comparator<? super T> comparator;

    public ArraySet() {
        this(Collections.emptyList(), null);
    }

    public ArraySet(Collection<? extends T> collection) {
        this(collection, null);
    }

    public ArraySet(Comparator<? super T> comparator) {
        this(Collections.emptyList(), comparator);
    }

    public ArraySet(Collection<? extends T> collection, Comparator<? super T> comparator) {
        NavigableSet<T> set = new TreeSet<>(comparator);
        set.addAll(collection);
        this.arrayList = new ReversedArrayList<>(set);
        this.comparator = checkNaturalOrder(comparator);
    }

    private ArraySet(ReversedArrayList<T> arrayList, Comparator<? super T> comparator) {
        this.arrayList = arrayList;
        this.comparator = comparator;
    }

    private T safeGet(int index) {
        if (index >= 0 && index < size()) {
            return arrayList.get(index);
        } else {
            return null;
        }
    }

    @Override
    public T lower(T t) {
        return safeGet(lowerIndex(t, false));
    }

    @Override
    public T floor(T t) {
        return safeGet(lowerIndex(t, true));
    }

    @Override
    public T ceiling(T t) {
        return safeGet(upperIndex(t, true));
    }

    @Override
    public T higher(T t) {
        return safeGet(upperIndex(t, false));
    }

    @Override
    public int size() {
        return arrayList.size();
    }

    @Override
    public boolean isEmpty() {
        return size() == 0;
    }

    @Override
    public boolean contains(Object o) {
        T element = (T) Objects.requireNonNull(o);
        return Collections.binarySearch(arrayList, element, comparator) >= 0;
    }

    @Override
    public Iterator<T> iterator() {
        return arrayList.iterator();
    }

    @Override
    public NavigableSet<T> descendingSet() {
        return new ArraySet<>(new ReversedArrayList<>(arrayList, true), Collections.reverseOrder(comparator));
    }

    @Override
    public Iterator<T> descendingIterator() {
        return descendingSet().iterator();
    }

    private int lowerIndex(T element, boolean inclusive) {
        int index = Collections.binarySearch(arrayList, Objects.requireNonNull(element), comparator);

        if (index < 0) {
            return -index - 2;
        } else {
            return inclusive ? index : index - 1;
        }
    }

    private int upperIndex(T element, boolean inclusive) {
        int index = Collections.binarySearch(arrayList, Objects.requireNonNull(element), comparator);

        if (index < 0) {
            return -index - 1;
        } else {
            return inclusive ? index : index + 1;
        }
    }

    @Override
    public NavigableSet<T> subSet(T fromElement, boolean fromInclusive, T toElement, boolean toInclusive) {
        if (compareElements(fromElement, toElement) > 0) {
            throw new IllegalArgumentException();
        }

        return subSetImpl(fromElement, fromInclusive, toElement, toInclusive);
    }

    private NavigableSet<T> subSetImpl(T fromElement, boolean fromInclusive, T toElement, boolean toInclusive) {
        Objects.requireNonNull(fromElement);
        Objects.requireNonNull(toElement);

        int firstIndex = upperIndex(fromElement, fromInclusive);
        int lastIndex = lowerIndex(toElement, toInclusive);

        return firstIndex > lastIndex ? new ArraySet<>(comparator)
                : new ArraySet<>(arrayList.subList(firstIndex, lastIndex + 1), comparator);
    }

    private int compareElements(T el1, T el2) {
        if (comparator == null) {
            Comparable<T> comparableEl1 = (Comparable<T>)el1;
            return comparableEl1.compareTo(el2);
        } else {
            return comparator.compare(el1, el2);
        }
    }

    @Override
    public NavigableSet<T> headSet(T toElement, boolean inclusive) {
        if (isEmpty()) {
            return this;
        }

        return subSetImpl(first(), true, toElement, inclusive);
    }

    @Override
    public NavigableSet<T> tailSet(T fromElement, boolean inclusive) {
        if (isEmpty()) {
            return this;
        }

        return subSetImpl(fromElement, inclusive, last(), true);
    }

    @Override
    public Comparator<? super T> comparator() {
        return comparator;
    }

    @Override
    public java.util.SortedSet<T> subSet(T fromElement, T toElement) {
        if (compareElements(fromElement, toElement) > 0) {
            throw new IllegalArgumentException();
        }

        return subSetImpl(fromElement, true, toElement, false);
    }

    @Override
    public java.util.SortedSet<T> headSet(T toElement) {
        return headSet(toElement, false);
    }

    @Override
    public java.util.SortedSet<T> tailSet(T fromElement) {
        return tailSet(fromElement, true);
    }

    @Override
    public T first() {
        return getFirstOrLast(0);
    }

    @Override
    public T last() {
        return getFirstOrLast(size() - 1);
    }

    private T getFirstOrLast(int index) {
        if (isEmpty()) {
            throw new NoSuchElementException();
        } else {
            return arrayList.get(index);
        }
    }


    private Comparator<? super T> checkNaturalOrder(Comparator<? super T> comparator) {
        if (Comparator.naturalOrder().equals(comparator)) {
            return null;
        } else {
            return comparator;
        }
    }

    //Unsupported operations.

    @Override
    public boolean add(T t) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean remove(Object o) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean addAll(Collection<? extends T> c) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean retainAll(Collection<?> c) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean removeAll(Collection<?> c) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void clear() {
        throw new UnsupportedOperationException();
    }

    @Override
    public T pollFirst() {
        throw new UnsupportedOperationException();
    }

    @Override
    public T pollLast() {
        throw new UnsupportedOperationException();
    }
}