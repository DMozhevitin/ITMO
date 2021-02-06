import java.io.*;
import java.util.Scanner;

public class SumHexFile {
    public static void main(String args[]) {
        try {
            Scanner scan = new Scanner(new File(args[0]), "UTF-8");

            int sum = 0;
            while (scan.hasNext()) {
                String s = scan.next();
                s = s.toLowerCase();

                try {
                    if (s.startsWith("0x")) {
                        sum += Integer.parseUnsignedInt(s.substring(2), 16);
                    } else {
                        sum += Integer.parseInt(s);
                    }

                } catch (NumberFormatException e) {
                    scan.close();
                    System.out.println(s + " Isn't a number!");
                    return;
                }
            }

            try {
                PrintWriter out = new PrintWriter(new File(args[1]), "UTF-8");
                out.println(sum);
                out.close();
                scan.close();
            } catch (ArrayIndexOutOfBoundsException e) {
                scan.close();
                System.out.println("Incorrect output file!");
            } catch (UnsupportedEncodingException e) {
                System.out.println("Unsupported encoding!");
            } catch (FileNotFoundException e) {
                System.out.println("Output file does not exist!");
            }

        } catch (FileNotFoundException e) {
            System.out.println("Input file does not exist");
        } catch (ArrayIndexOutOfBoundsException e) {
            System.out.println("Incorrect input file!");
        }

    }
}
