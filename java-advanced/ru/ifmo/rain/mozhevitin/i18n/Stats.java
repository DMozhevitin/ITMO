package ru.ifmo.rain.mozhevitin.i18n;

import java.text.BreakIterator;
import java.util.*;

public class Stats<T extends Comparable<? super T>> {
	//finals?
    private Map<T, Integer> cnt;
    private Set<T> unique;
    private T min;
    private T max;
    private int minLength;
    private String withMinLength;
    private int maxLength;
    private String withMaxLength;
    private int sumLength;
    private Locale locale;

    Stats(Locale locale) {
        this.cnt = new HashMap<>();
        this.unique = new HashSet<>();
        min = null;
        max = null;
        minLength = 0;
        maxLength = 0;
        sumLength = 0;
        this.locale = locale;
    }

    void update(T element) {
        if (element == null) {
            return;
        }

        int count = cnt.getOrDefault(element, 0);
        cnt.put(element, count + 1);

        unique.add(element);

        if (min == null || element.compareTo(min) < 0) {
            min = element;
        }

        if (max == null || max.compareTo(element) < 0) {
            max = element;
        }

        int length = localizedLength(element.toString());

        if (minLength == 0 || length < minLength) {
            minLength = length;
            withMinLength = element.toString();
        }

        if (length > maxLength) {
            maxLength = length;
            withMaxLength = element.toString();
        }

        sumLength += localizedLength(element.toString());
    }

    protected int localizedLength(String s) {
        BreakIterator it = BreakIterator.getCharacterInstance(locale);
        it.setText(s);

        int index = it.first();
        int cntIters = 0;
        while (index != BreakIterator.DONE) {
            cntIters++;
            index = it.next();
        }

        return cntIters - 1;
    }


    public Set<T> getUnique() {
        return unique;
    }

    public Map<T, Integer> getCnt() {
        return cnt;
    }

    public String getWithMinLength() {
        return withMinLength;
    }

    public String getWithMaxLength() {
        return withMaxLength;
    }

    public T getMin() {
        return min;
    }

    public T getMax() {
        return max;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Stats)) return false;
        Stats<?> stats = (Stats<?>) o;
        return getMinLength() == stats.getMinLength() &&
                getMaxLength() == stats.getMaxLength() &&
                Objects.equals(getMin(), stats.getMin()) &&
                Objects.equals(getMax(), stats.getMax()) &&
                locale.equals(stats.locale);
    }

    @Override
    public int hashCode() {
        return Objects.hash(getCnt(), getUnique(), getMin(), getMax(), getMinLength(), getWithMinLength(), getMaxLength(), getWithMaxLength(), sumLength, locale);
    }

    public Integer getMinLength() {
        return minLength;
    }

    public Integer getMaxLength() {
        return maxLength;
    }

    public double getAvg() {
        return (double)sumLength / TotalStats.totalCount(this);
    }

    public void setMin(T min) {
        this.min = min;
    }

    public void setMax(T max) {
        this.max = max;
    }

    public void setMinLength(int minLength) {
        this.minLength = minLength;
    }

    public void setWithMinLength(String withMinLength) {
        this.withMinLength = withMinLength;
    }

    public void setMaxLength(int maxLength) {
        this.maxLength = maxLength;
    }

    public void setWithMaxLength(String withMaxLength) {
        this.withMaxLength = withMaxLength;
    }

    public void setSumLength(int sumLength) {
        this.sumLength = sumLength;
    }
}