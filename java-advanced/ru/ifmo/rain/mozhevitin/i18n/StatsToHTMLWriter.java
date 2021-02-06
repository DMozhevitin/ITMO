package ru.ifmo.rain.mozhevitin.i18n;

import ru.ifmo.rain.mozhevitin.i18n.bundle.AbstractStatsResourceBundle;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.text.MessageFormat;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.stream.Collectors;

public class StatsToHTMLWriter implements AutoCloseable {
    private static final String BUNDLES_PATH = String.join(".",
            "ru", "ifmo", "rain", "mozhevitin", "i18n", "bundle.");
    private static final String BOLD = "b";
    private static final String PARAGRAPH = "p";
    private static final String HEAD = "<head> <meta charset=\"UTF-8\"> </head>";
    private static final String EOLN = System.lineSeparator();

    private final BufferedWriter writer;

    StatsToHTMLWriter(Path out) throws IOException {
        writer = Files.newBufferedWriter(out, StandardCharsets.UTF_8);
    }

    void writeStats(TotalStats stats, Locale locale) throws IOException {
        AbstractStatsResourceBundle bundle = (AbstractStatsResourceBundle) ResourceBundle.getBundle(BUNDLES_PATH +
                "StatsResourceBundle", locale);
        if (bundle == null) {
            throw new IllegalArgumentException("Only RU and EN locales supported.");
        }


        String title = MessageFormat.format("{0}: {1}", bundle.getString("Analyzed file"), stats.getFilename());
        String total = totalStatsToHtml(stats, bundle);

        String statsPrefix = bundle.getString("Stats") + " ";
        List<String> statsByCategories = List.of(
                statsToHtml(stats.getWordStats(), bundle,
                        statsPrefix + bundle.getString("by words"), false),
                statsToHtml(stats.getLineStats(), bundle,
                        statsPrefix + bundle.getString("by lines"), false),
                statsToHtml(stats.getSentenceStats(), bundle,
                        statsPrefix + bundle.getString("by sentences"), false),
                statsToHtml(stats.getDateStats(), bundle,
                        statsPrefix + bundle.getString("by dates"), false),
                statsToHtml(stats.getNumStats(), bundle,
                        statsPrefix + bundle.getString("by numbers"), true),
                statsToHtml(stats.getCurrencyStats(), bundle,
                        statsPrefix + bundle.getString("by currency"), true)
        );

        StringBuilder sbHtml = new StringBuilder();
        sbHtml.append(HEAD)
                .append(EOLN)
                .append(wrapTag(title, "h1"))
                .append(total)
                .append(EOLN);
        statsByCategories.forEach(s -> sbHtml.append(s).append(EOLN));

        writer.write(sbHtml.toString());
    }

    private String wrapTag(String html, String tag) {
        return MessageFormat.format("<{0}>", tag) +
                html +
                MessageFormat.format("</{0}>", tag);
    }

    private String wrapBold(String html) {
        return wrapTag(html, BOLD);
    }

    private String wrapParagraph(String html) {
        return wrapTag(html, PARAGRAPH);
    }

    private String totalStatsToHtml(TotalStats stats, ResourceBundle bundle) {
        String[] lines = {
                wrapTag(wrapBold(bundle.getString("General stats")), "h2"),
                statsRow2Html(stats.getSentencesCount(), bundle, "Sentences count"),
                statsRow2Html(stats.getLinesCount(), bundle, "Lines count"),
                statsRow2Html(stats.getWordsCount(), bundle, "Words count"),
                statsRow2Html(stats.getNumsCount(), bundle, "Nums count"),
                statsRow2Html(stats.getCurrenciesCount(), bundle, "Currencies count"),
                statsRow2Html(stats.getDatesCount(), bundle, "Dates count"),
        };

        return Arrays.stream(lines).collect(Collectors.joining(System.lineSeparator()));
    }

    private <T extends Comparable<? super T>> String statsToHtml(Stats<T> stats,
                                                                 AbstractStatsResourceBundle bundle,
                                                                 String title,
                                                                 boolean isNumeric) {
        String[] lines = {
                wrapTag(title, "h2"),
                wrapParagraph(MessageFormat.format("{0}: {1} ({2} {3})",
                        bundle.getString("Count"),
                        TotalStats.totalCount(stats),
                        stats.getUnique().size(),
                        bundle.uniqueElements(stats.getUnique().size()))),
                statsRow2Html(stats.getMax(), bundle, "Max"),
                statsRow2Html(stats.getMin(), bundle, "Min"),
                statsRow2Html(stats.getMinLength(), stats.getWithMinLength(), bundle, "Min length"),
                statsRow2Html(stats.getMaxLength(), stats.getWithMaxLength(), bundle, "Max length"),
                isNumeric
                        ? statsRow2Html(stats.getAvg(), bundle, "Avg value")
                        : statsRow2Html(stats.getAvg(), bundle, "Avg length")
        };

        return Arrays.stream(lines).collect(Collectors.joining(System.lineSeparator()));
    }

    private <T> String statsRow2Html(T value, ResourceBundle bundle, String key) {
        return wrapParagraph(MessageFormat.format("{0}: {1}", bundle.getString(key), safeToString(value)));
    }

    private <T> String statsRow2Html(T value1, String value2, ResourceBundle bundle, String key) {
        if (value1.toString().isEmpty() || value1.toString().equals("0")) {
            return statsRow2Html(value1, bundle, key);
        } else {
            return wrapParagraph(MessageFormat.format("{0}: {1} ({2})", bundle.getString(key), safeToString(value1), value2));
        }
    }

    private <T> String safeToString(T x) {
        return x == null ? "N/A" : x.toString();
    }

    @Override
    public void close() throws IOException {
        writer.close();
    }
}