package md2html;

import java.io.IOException;

public class Md2Html {
    public static void main(String[] args) throws IOException {
        MarkdownFileSource source = new MarkdownFileSource(args[0], args[1]);
        Md2HtmlParser parser = new Md2HtmlParser(source);
        source.write(parser.parse());
        source.close();
    }
}
