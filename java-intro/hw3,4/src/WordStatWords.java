import java.io.*;
import java.util.*;

public class WordStatWords {
    public static void main(String args[]) {
        try {
            Scanner scan = new Scanner(new File(args[0]), "UTF-8");
            Map<String, Integer> map = new TreeMap<String, Integer>();

            while (scan.hasNextLine()) {
                String s = scan.nextLine().toLowerCase();
                String[] parsed = s.split("[^\\p{L}\\p{Pd}']");

                for (String elem : parsed) {
                    if (elem.length() > 0) {
                        if (map.containsKey(elem)) {
                            map.put(elem, map.get(elem) + 1);
                        } else {
                            map.put(elem, 1);
                        }
                    }
                }
            }

            try {
                PrintWriter writer = new PrintWriter(new File(args[1]), "UTF-8");
                ArrayList<String> list = new ArrayList<String>(map.keySet());
                for (String i : list) {
                    writer.println(i + " " + map.get(i));
                }
                scan.close();
                writer.close();
            } catch (ArrayIndexOutOfBoundsException e) {
                scan.close();
                System.out.println("Incorrect output file!");
            } catch (UnsupportedEncodingException e) {
                System.out.println("Incorrect encoding!");
            }

        } catch (FileNotFoundException e) {
            System.out.println("Input file doesn't exist!");
        } catch (ArrayIndexOutOfBoundsException e) {
            System.out.println("Incorrect input file!");
        }

    }
}