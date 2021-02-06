package ru.ifmo.rain.mozhevitin.i18n;


import java.io.BufferedReader;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.text.BreakIterator;
import java.text.DateFormat;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.*;

public class TextStatistics {
    private final Locale locale;
    private final NumberFormat numberFormat;
    private final NumberFormat currencyFormat;
    private final List<DateFormat> dateFormats;

    public TextStatistics(Locale locale) {
        this.locale = locale;
        this.numberFormat = NumberFormat.getNumberInstance(locale);
        this.currencyFormat = NumberFormat.getCurrencyInstance(locale);
        this.dateFormats = List.of(
                DateFormat.getDateInstance(DateFormat.SHORT, locale),
                DateFormat.getDateInstance(DateFormat.DEFAULT, locale),
                DateFormat.getDateInstance(DateFormat.FULL, locale),
                DateFormat.getDateInstance(DateFormat.LONG, locale),
                DateFormat.getDateInstance(DateFormat.MEDIUM, locale));
    }

    public TotalStats getStatistics(String inputPath) {
        NumericStats numStats = new NumericStats(locale);
        NumericStats currencyStats = new NumericStats(locale);
        Stats<String> lineStats = new Stats<>(locale);
        Stats<String> sentenceStats = new Stats<>(locale);
        Stats<String> wordStats = new Stats<>(locale);
        Stats<Date> dateStats = new Stats<>(locale);

        List<String> lines = new ArrayList<>();
        try (BufferedReader in = Files.newBufferedReader(Path.of(inputPath), StandardCharsets.UTF_8)) {
            String line = in.readLine();
            while (line != null) {
                lines.add(line);
                line = in.readLine();
            }
        } catch (IOException e) {
            System.err.println("Cannot read input file.");
            return null;
        }

        lines.forEach(lineStats::update);
        String text = String.join("", lines);

        wordLevelStats(text, numStats, currencyStats, dateStats, wordStats);
        getStatsByLevel(text, sentenceStats, BreakIterator.getSentenceInstance(locale));

        return TotalStats.of(inputPath, numStats, currencyStats, lineStats, sentenceStats, wordStats, dateStats);
    }

    private void wordLevelStats(String text, Stats<Double> numStats, Stats<Double> currencyStats,
                                Stats<Date> dateStats, Stats<String> wordStats) {

        BreakIterator iterator = BreakIterator.getWordInstance(locale);
        iterator.setText(text);

        for (int index = iterator.first(), prevIndex = 0;
             index != BreakIterator.DONE;
             prevIndex = index, index = iterator.next()) {

            String word = text.substring(prevIndex, index).trim();
            boolean isDate = tryParseDate(word, dateStats);
            boolean isCurrency = false, isNumber = false;
            if (!isDate) {
                isCurrency = tryParseNumericToken(word, currencyStats, currencyFormat);
                isNumber = tryParseNumericToken(word, numStats, numberFormat);
            }

            if (!isDate && !isCurrency && !isNumber) {
                processWord(word, wordStats);
            }
        }
    }

    private void getStatsByLevel(String text, Stats<String> stats, BreakIterator it) {
        it.setText(text);
        for (int index = it.first(), prevIndex = 0;
             index != BreakIterator.DONE;
             prevIndex = index, index = it.next()) {
            String token = text.substring(prevIndex, index).trim();

            if (!token.isEmpty()) {
                stats.update(token);
            }

        }
    }


    private void processWord(String word, Stats<String> wordStats) {
        if (word == null || word.isEmpty()) {
            return;
        }

        if (word.length() > 1 || Character.isLetter(word.charAt(0))) {
            wordStats.update(word);
        }
    }

    private boolean tryParseDate(String word, Stats<Date> dateStats) {
        try {
            for (DateFormat dateFormat : dateFormats) {
                Date date = dateFormat.parse(word);
                dateStats.update(date);
                return true;
            }
        } catch (ParseException ignored) {
        }

        return false;
    }

    private boolean tryParseNumericToken(String word, Stats<Double> stats, NumberFormat format) {
        try {
            Double currency = format.parse(word).doubleValue();
            stats.update(currency);
        } catch (ParseException e) {
            return false;
        }

        return true;
    }

    public static void main(String[] args) throws IOException {
        if (args.length != 4 || Arrays.stream(args).anyMatch(Objects::isNull)) {
            System.err.println("Invalid arguments.");
        }

        Locale textLocale = new Locale(args[0]);
        Locale outputLocale = new Locale(args[1]);
        String inputFilePath = args[2];
        String outputFilePath = args[3];

        TextStatistics stat = new TextStatistics(textLocale);
        TotalStats stats = stat.getStatistics(inputFilePath);

        if (stats == null) {
            return;
        }

        try (StatsToHTMLWriter writer = new StatsToHTMLWriter(Path.of(outputFilePath))) {
            writer.writeStats(stats, outputLocale);
        }
    }
}
