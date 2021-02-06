package ru.ifmo.rain.mozhevitin.i18n.bundle;

import java.text.ChoiceFormat;

public class StatsResourceBundle_ru extends AbstractStatsResourceBundle {
    private final double[] limits = {0, 1, 2, 5, 21};
    private final String[] uniques =
            {"уникальных значений", "уникальное значение", "уникальных значения",
            "уникальных значений", "уникальных значений"};
    private final ChoiceFormat choiceFormat = new ChoiceFormat(limits, uniques);

    private static final Object[][] CONTENTS = {
            {"Analyzed file", "Анализируемый файл"},
            {"General stats", "Сводная статистика"},
            {"Stats", "Статистика"},
            {"by words", "по словам"},
            {"by sentences", "по предложениям"},
            {"by lines", "по строкам"},
            {"by currency", "по деньгам"},
            {"by numbers", "по числам"},
            {"by dates", "по датам"},

            {"Sentences count", "Количество предложений"},
            {"Lines count", "Количество строк"},
            {"Nums count", "Количество чисел"},
            {"Dates count", "Количество дат"},
            {"Currencies count", "Количество денежных единиц"},
            {"Words count", "Количество слов"},

            {"Count", "Количество"},
            {"Min", "Минимум"},
            {"Max", "Максимум"},
            {"Min length", "Минимальная длина"},
            {"Max length", "Максимальная длина"},
            {"Avg length", "Средняя длина"},
            {"Avg value", "Среднее значение"},

    };

    @Override
    protected Object[][] getContents() {
        return CONTENTS;
    }

    @Override
    public String uniqueElements(int n) {
        if (n >= 21) {
            n %= 10;
        }
        return choiceFormat.format(n);
    }
}
