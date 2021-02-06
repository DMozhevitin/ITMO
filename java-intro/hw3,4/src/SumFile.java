import java.io.*;
import java.util.Scanner;
import java.util.InputMismatchException;

public class SumFile {
    public static void main(String args[]) {
        try {
            Scanner scan = new Scanner(new File(args[0]), "UTF-8");
            PrintStream out = new PrintStream(new FileOutputStream(args[1]));

            int sum = 0;
            while (scan.hasNext()) {
                try {
                    sum += scan.nextInt();
                } catch (InputMismatchException e) {
                    String s = scan.next();
                }
            }
            out.println(sum);
            out.close();
            scan.close();
        } catch (FileNotFoundException e) {

        }
    }
}
