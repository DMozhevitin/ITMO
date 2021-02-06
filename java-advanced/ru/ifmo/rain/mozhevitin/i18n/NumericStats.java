package ru.ifmo.rain.mozhevitin.i18n;

import java.util.Locale;
import java.util.Objects;

public class NumericStats extends Stats<Double> {
    private double sum;

    NumericStats(Locale locale) {
        super(locale);
        sum = 0;
    }

    @Override
    public double getAvg() {
        return sum / TotalStats.totalCount(this);
    }

    @Override
    void update(Double element) {
        super.update(element);
        sum += element;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof NumericStats)) return false;
        if (!super.equals(o)) return false;
        NumericStats that = (NumericStats) o;
        return sum == that.sum;
    }


    @Override
    protected int localizedLength(String s) {
        Double x = Double.parseDouble(s);

        if (x == x.intValue()) {
            return super.localizedLength(String.valueOf(x.intValue()));
        } else {
            return super.localizedLength(s);
        }
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), sum);
    }

    public void setSum(int sum) {
        this.sum = sum;
    }
}
