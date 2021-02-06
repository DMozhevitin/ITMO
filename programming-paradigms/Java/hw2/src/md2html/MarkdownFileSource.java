package md2html;


import java.io.*;
import java.nio.charset.StandardCharsets;

public class MarkdownFileSource {
    private BufferedReader reader;
    private BufferedWriter writer;
    private char END = '\0';
    private char c;
    private String line = "";
    private StringBuilder paragraph = new StringBuilder();

    public MarkdownFileSource(final String inputFileName, final String outputFileName) {
        try {
            reader = new BufferedReader(new FileReader(inputFileName, StandardCharsets.UTF_8));
        } catch (final IOException e) {
            System.out.println("There are some problems with input file");
        }

        try {
            writer = new BufferedWriter(new FileWriter(outputFileName, StandardCharsets.UTF_8));
        } catch (final IOException e) {
            System.out.println("There are some problems with output file");
        }
    }

    private String nextLine() throws IOException {
        return line = reader.readLine();
    }

    public String getLine() {
        return line;
    }

    public String getParagraph() {
        return paragraph.toString();
    }

    public char getChar() {
        return c;
    }

    private char readChar() throws IOException {
        final int read = reader.read();
        return read == -1 ? END : (char) read;
    }

    public String nextParagraph() throws IOException {
        if (line == null) {
            return "";
        }

        while (line != null && line.isEmpty()) {
            line = nextLine();
        }

        paragraph = new StringBuilder();
        paragraph.append(line);

        while (nextLine() != null && !getLine().isEmpty()) {
            paragraph.append('\n');
            paragraph.append(getLine());
        }

        while (line != null && line.isEmpty()) {
            nextLine();
        }

        return paragraph.toString();
    }

    public char nextChar() throws IOException {
        c = readChar();
        return c;
    }

    public void write(final String out) throws IOException {
        writer.write(out);
    }

    public void close() throws IOException {
        reader.close();
        writer.close();
    }
}
