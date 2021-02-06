package ru.ifmo.rain.mozhevitin.i18n;

import java.util.Date;
import java.util.Objects;
import java.util.function.Function;

public class TotalStats {
	//finals?
    private NumericStats numStats;
    private NumericStats currencyStats;
    private Stats<String> lineStats;
    private Stats<String> sentenceStats;
    private Stats<String> wordStats;
    private Stats<Date> dateStats;

    private String filename;
    private int wordsCount;
    private int numsCount;
    private int currenciesCount;
    private int linesCount;
    private int sentencesCount;
    private int datesCount;

    private TotalStats() {}

    private TotalStats(String filename,
                      NumericStats numStats,
                      NumericStats currencyStats,
                      Stats<String> lineStats,
                      Stats<String> sentenceStats,
                      Stats<String> wordStats,
                      Stats<Date> dateStats) {
        this.numStats = numStats;
        this.currencyStats = currencyStats;
        this.lineStats = lineStats;
        this.sentenceStats = sentenceStats;
        this.wordStats = wordStats;
        this.dateStats = dateStats;
        this.filename = filename;

        wordsCount = totalCount(wordStats);
        currenciesCount = totalCount(currencyStats);
        linesCount = totalCount(lineStats);
        sentencesCount = totalCount(sentenceStats);
        datesCount = totalCount(dateStats);
        numsCount = totalCount(numStats);
    }

    public static <T extends Comparable<? super T>> int totalCount(Stats<T> stats) {
        return stats == null ? 0 : stats.getCnt()
                .values()
                .stream()
                .mapToInt(c -> c)
                .sum();
    }

    public NumericStats getNumStats() {
        return numStats;
    }

    public NumericStats getCurrencyStats() {
        return currencyStats;
    }

    public Stats<String> getLineStats() {
        return lineStats;
    }

    public Stats<String> getSentenceStats() {
        return sentenceStats;
    }

    public String getFilename() {
        return filename;
    }

    public Stats<String> getWordStats() {
        return wordStats;
    }

    public Stats<Date> getDateStats() {
        return dateStats;
    }

    public static TotalStats of (String filename,
                                 NumericStats numStats,
                                 NumericStats currencyStats,
                                 Stats<String> lineStats,
                                 Stats<String> sentenceStats,
                                 Stats<String> wordStats,
                                 Stats<Date> dateStats) {
        return new TotalStats(filename, numStats, currencyStats, lineStats, sentenceStats, wordStats, dateStats);
    }

    public int getWordsCount() {
        return wordsCount;
    }

    public int getNumsCount() {
        return numsCount;
    }

    public int getCurrenciesCount() {
        return currenciesCount;
    }

    public int getLinesCount() {
        return linesCount;
    }

    public int getSentencesCount() {
        return sentencesCount;
    }

    public int getDatesCount() {
        return datesCount;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof TotalStats)) return false;
        TotalStats stats = (TotalStats) o;
        return getWordsCount() == stats.getWordsCount() &&
                getNumsCount() == stats.getNumsCount() &&
                getCurrenciesCount() == stats.getCurrenciesCount() &&
                getLinesCount() == stats.getLinesCount() &&
                getSentencesCount() == stats.getSentencesCount() &&
                getDatesCount() == stats.getDatesCount() &&
                getNumStats().equals(stats.getNumStats()) &&
                getCurrencyStats().equals(stats.getCurrencyStats()) &&
                getLineStats().equals(stats.getLineStats()) &&
                getSentenceStats().equals(stats.getSentenceStats()) &&
                getWordStats().equals(stats.getWordStats()) &&
                getDateStats().equals(stats.getDateStats()) &&
                getFilename().equals(stats.getFilename());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getNumStats(), getCurrencyStats(), getLineStats(),
                getSentenceStats(), getWordStats(), getDateStats(), getFilename(),
                getWordsCount(), getNumsCount(), getCurrenciesCount(), getLinesCount(),
                getSentencesCount(), getDatesCount());
    }
}
