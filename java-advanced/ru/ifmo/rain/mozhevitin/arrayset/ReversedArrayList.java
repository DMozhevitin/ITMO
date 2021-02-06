package ru.ifmo.rain.mozhevitin.arrayset;

import java.util.*;

public class ReversedArrayList<T> extends AbstractList<T> implements RandomAccess {
    private final List<T> arrayList;
    private boolean isReversed;

    ReversedArrayList(Collection<T> collection) {
        this.arrayList = List.copyOf(collection);
        this.isReversed = false;
    }

    private ReversedArrayList(final List<T> list) {
        this.arrayList = Collections.unmodifiableList(list);
        this.isReversed = false;
    }

    ReversedArrayList(final ReversedArrayList<T> list, final boolean isReversed) {
        this.arrayList = List.copyOf(list.arrayList);
        this.isReversed = list.isReversed ^ isReversed;
    }

    @Override
    public T get(int index) {
        return arrayList.get(rightIndex(index));
    }

    @Override
    public ReversedArrayList<T> subList(int fromIndex, int toIndex) {
        if (isReversed) {
            return new ReversedArrayList<>(arrayList.subList(reversedIndex(
                    toIndex - 1), reversedIndex(fromIndex) + 1));
        } else {
            return new ReversedArrayList<>(arrayList.subList(fromIndex, toIndex));
        }
    }

    @Override
    public int size() {
        return arrayList.size();
    }

    private int reversedIndex(int index) {
        return arrayList.size() - index - 1;
    }

    private int rightIndex(int index) {
        if (isReversed) {
            return reversedIndex(index);
        } else {
            return index;
        }
    }
}

