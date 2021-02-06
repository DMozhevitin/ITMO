import java.io.*;
import java.util.*;
import java.util.Scanner;
import java.util.LinkedHashMap;
import java.util.ArrayList;

public class WordStatInput {

    public static void main(String args[]) {
        try {
            Scanner scanner = new Scanner(new File(args[0]), "UTF-8");
            PrintStream out = new PrintStream(new FileOutputStream(args[1]));

            LinkedHashMap<String, Integer> map = new LinkedHashMap<String, Integer>();

            while (scanner.hasNextLine()) {
                String string = scanner.nextLine().toLowerCase();
                String[] parsedString = string.split("[^\\p{L}\\p{Pd}']");

                for (int i = 0; i < parsedString.length; i++) {
                    if (!parsedString[i].equals(""))
                        map.put(parsedString[i], (map.containsKey(parsedString[i]) ? map.get(parsedString[i]) + 1 : 1));
                }
            }

            ArrayList<String> keyList = new ArrayList<String>(map.keySet());
            for (String i : keyList) {
                out.println(i + " " + map.get(i));
            }

            out.close();
            scanner.close()

        } catch (FileNotFoundException e) {

        }


    }
}