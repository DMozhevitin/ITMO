import java.io.*;
import java.util.ArrayList;
import java.util.InputMismatchException;

public class MyScanner {
    private InputStream stream;
    final int BUFFER_SIZE = 256;
    final char EOLN = '\n';
    byte[] buf = new byte[BUFFER_SIZE];
    int now = BUFFER_SIZE;
    int stream_end = BUFFER_SIZE;
    public int mxlength = 0;

    public MyScanner(InputStream stream) {
        this.stream = stream;
    }

    public MyScanner() {
        this.stream = new DataInputStream(System.in);
    }

    public MyScanner(String InputFile) throws FileNotFoundException {
        this.stream = new FileInputStream(InputFile);
    }

    public boolean hasNext() throws IOException {
        return (now < stream_end || stream.available() > 0);
    }

    public char nextChar() throws IOException {
        if (now == stream_end) {
            now = 0;
            stream_end = stream.read(buf);
        }
        return (char)buf[now++];
    }

    public String nextLine() throws IOException {
        StringBuilder s = new StringBuilder();

        while (hasNext()) {
            char c = nextChar();
            if (c == EOLN) {
                break;
            }

            s.append(c);
        }

        return s.toString();
    }

    public ArrayList<Integer> nextArray() throws IOException {
        ArrayList<Integer> a = new ArrayList<Integer>();
        String s = nextLine();

        if (s.length() == 0) {
            return a;
        }

        String[] splitted = s.split("[^\\p{L}\\p{Pd}']");
        mxlength = Math.max(mxlength, splitted.length);

        for (String elem : splitted) {
            a.add(Integer.parseInt(elem));
        }

        return a;
    }

    public int nextInt() throws IOException {
        char c = nextChar();
        boolean negative = false;
        int res = 0;

        if (!Character.isDigit(c) && c != '-') {
            throw new IOException();
        }

        if (c == '-') {
            negative = true;
        } else {
            res = (c - '0');
        }

        while (hasNext()) {
            c = nextChar();
            if (!Character.isDigit(c) && !Character.isWhitespace(c)) {
                throw new IOException();
            } else if (Character.isWhitespace(c)) {
                return res;
            }

            res = res * 10 + (c - '0');
        }

        return (negative ? -res : res);
    }
}