import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;

public class MyScanner implements AutoCloseable {
    private InputStreamReader in;
    private final int BUFFER_SIZE = 2048;
    private char[] buffer = new char[BUFFER_SIZE];
    private int now = 0, cnt = 0;

    public MyScanner(String inputFile) throws IOException {
        in = new InputStreamReader(new FileInputStream(inputFile), StandardCharsets.UTF_8);
    }

    private boolean stream_end() {
        return now == cnt;
    }

    private void read() throws IOException {
        do {
            cnt = in.read(buffer, now = 0, BUFFER_SIZE);
        } while (cnt == 0);
    }

    private int nextChar() throws IOException {
        if (stream_end()) {
            read();
        }

        if (cnt == -1) {
            return -1;
        }

        return buffer[now++];
    }

    public boolean hasNext() throws IOException {
        if (stream_end()) {
            read();
        }

        return cnt != -1;
    }

    public String nextLine() throws IOException {
        StringBuilder s = new StringBuilder();
        int c = nextChar();

        while (c != -1) {
            if (c == '\n') {
                return s.toString();
            }
            s.append((char) c);
            c = nextChar();
        }

        return s.toString();
    }

    public ArrayList<String> nextArray() throws IOException {
        ArrayList<String> a = new ArrayList<>();
        String s = nextLine().toLowerCase();

        StringBuilder tmp = new StringBuilder();

        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            while (Character.isLetter(c) || Character.getType(c) == Character.DASH_PUNCTUATION || c == '\'') {
                tmp.append(c);
                i++;
                c = s.charAt(i);
            }

            if (!tmp.toString().isEmpty()) {
                a.add(tmp.toString());
                tmp.setLength(0);
            }
        }

        return a;
    }

    public void close() throws IOException {
        in.close();
    }
}
