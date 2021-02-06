package md2html;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class Md2HtmlParser {
    MarkdownFileSource source;
    private String currentParagraph;
    private int index = 0;
    final private Map<String, String> closeSequences = new HashMap<>();
    final private Map<Character, String> specialSymbols = new HashMap<>();
    final private Map<String, String> openClose = new HashMap<>();
    final private String[] markupSequences = {"**", "*", "__", "_", "--", "`", "++", "~"};

    public Md2HtmlParser(MarkdownFileSource source) {
        this.source = source;
        openClose.put("</strong>", "<strong>");
        openClose.put("</em>", "<em>");
        openClose.put("</code>", "<code>");
        openClose.put("</s>", "<s>");
        openClose.put("</u>", "<u>");
        openClose.put("</mark>", "<mark>");
        specialSymbols.put('>', "&gt;");
        specialSymbols.put('<', "&lt;");
        specialSymbols.put('&', "&amp;");
        closeSequences.put("**", "</strong>");
        closeSequences.put("__", "</strong>");
        closeSequences.put("*", "</em>");
        closeSequences.put("_", "</em>");
        closeSequences.put("--", "</s>");
        closeSequences.put("++", "</u>");
        closeSequences.put("`", "</code>");
        closeSequences.put("~", "</mark>");
        closeSequences.put(")", "");
        closeSequences.put("]", "");
    }

    public String parse() throws IOException {
        StringBuilder parsedMarkdownText = new StringBuilder();
        while (!source.nextParagraph().isEmpty()) {
            currentParagraph = source.getParagraph();
            parsedMarkdownText.append(parseParagraph()).append('\n');
        }

        return parsedMarkdownText.toString();
    }

    private String parseParagraph() {
        index = 0;
        StringBuilder parsedParagraph = new StringBuilder();
        int countOfSharps = parseHeadline();

        if (countOfSharps == 0) {
            parsedParagraph.append("<p>");
            parsedParagraph.append(currentParagraph, 0, index);
            parsedParagraph.append(parseText(null, false));
            parsedParagraph.append("</p>");
        } else {
            parsedParagraph.append("<h").append(countOfSharps).append(">");
            parsedParagraph.append(parseText(null, false));
            parsedParagraph.append("</h").append(countOfSharps).append(">");
        }

        return parsedParagraph.toString();
    }

    private int parseHeadline() {
        while (index < currentParagraph.length() && currentParagraph.charAt(index) == '#') {
            index++;
        }


        if (Character.isWhitespace(currentParagraph.charAt(index))) {
            return index++;
        }

        return 0;
    }

    private String parseText(String stopSequence, boolean inPicture) {
        StringBuilder parsedText = new StringBuilder();
        while (index < currentParagraph.length()) {
            if (index > 0 && currentParagraph.charAt(index - 1) == '\\') {
                parsedText.append(convertSpecialMarkdownSymbols());
            } else if (checkForStopSequence(stopSequence)) {
                parsedText.append(convertStopSequence(stopSequence));
                index += stopSequence.length();
                return parsedText.toString();
            } else if (currentParagraph.charAt(index) == '!') {
                parsedText.append(convertPicture());
            } else if (!inPicture && currentParagraph.charAt(index) == '[') {
                parsedText.append(convertLink());
            } else if (!inPicture && getMarkupSequence() != null) {
                parsedText.append(convertMarkup());
            } else if (currentParagraph.charAt(index) == '\\') {
                if (index + 1 >= currentParagraph.length()) {
                    parsedText.append('\\');
                }
            } else {
                parsedText.append(convertSpecialHtmlSymbols(currentParagraph.charAt(index)));
            }
            index++;
        }

        return parsedText.toString();
    }


    private boolean checkForStopSequence(String stopSequence) {
        if (stopSequence == null || index + stopSequence.length() > currentParagraph.length()) {
            return false;
        }
        return currentParagraph.substring(index, index + stopSequence.length()).equals(stopSequence);
    }

    private String convertStopSequence(String stopSequence) {
        return closeSequences.get(stopSequence);
    }

    private String getMarkupSequence() {
        for (String sequence : markupSequences) {
            if (index + sequence.length() <= currentParagraph.length() &&
                    currentParagraph.substring(index, index + sequence.length()).equals(sequence)) {
                return sequence;
            }
        }

        return null;
    }

    private String convertSpecialHtmlSymbols(char symbol) {
        return specialSymbols.containsKey(symbol) ? specialSymbols.get(symbol) : String.valueOf(symbol);
    }

    private String convertPicture() {
        StringBuilder convertedPicture = new StringBuilder();

        index += 2;
        String text = parseText("]", true);
        index++;
        String url = parseText(")", true);
        index--;
        convertedPicture.append("<img alt='").append(text).append("' src='").append(url).append("'>");

        return convertedPicture.toString();
    }

    private String convertLink() {
        StringBuilder convertedLink = new StringBuilder();

        index++;
        String link = parseText("]", false);
        index++;
        String url = parseText(")", true);
        convertedLink.append("<a href='").append(url).append("'>").append(link).append("</a>");
        index--;
        return convertedLink.toString();
    }

    private String convertMarkup() {
        StringBuilder convertedMarkup = new StringBuilder();

        String markupSequence = getMarkupSequence();
        index += markupSequence.length();

        int returnIndex = index;
        boolean needToReturn = false;

        String parsedText = parseText(markupSequence, false);
        int closeSequencePos = parsedText.length() - convertStopSequence(markupSequence).length();
        if (closeSequencePos > 0) {
            String closeSequence = parsedText.substring(parsedText.length() - convertStopSequence(markupSequence).length());
            convertedMarkup.append(openClose.getOrDefault(closeSequence, markupSequence));
            index--;
        } else {
            convertedMarkup.append(markupSequence);
            needToReturn = true;
        }

        if (needToReturn) {
            index = returnIndex - 1;
        } else
            convertedMarkup.append(parsedText);

        return convertedMarkup.toString();
    }

    private String convertSpecialMarkdownSymbols() {
        StringBuilder result = new StringBuilder();

        char currentChar = currentParagraph.charAt(index);
        if (currentChar == '\\') {
            result.append(currentChar);
        } else if (currentChar == '`' || currentChar == '_' || currentChar == '*') {
            result.append(currentChar);
        } else {
            result.append('\\');
            result.append(currentChar);
        }

        return result.toString();
    }
}
