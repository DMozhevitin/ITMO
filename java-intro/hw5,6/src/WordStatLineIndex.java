import java.nio.charset.StandardCharsets;
import java.util.*;
import java.lang.*;
import java.io.*;

public class WordStatLineIndex {
    public static void main(String[] args) {
        if (args.length > 1) {
            Map<String, ArrayList<String>> map = new TreeMap<>();
            try (MyScanner scan = new MyScanner(args[0])) {
                int cnt = 1;
                while (scan.hasNext()) {
                    try {
                        ArrayList<String> a = scan.nextArray();
                        for (int i = 0; i < a.size(); i++) {
                            String elem = a.get(i);
                            if (elem.length() > 0) {
                                ArrayList<String> tmp = map.getOrDefault(elem, new ArrayList<>());
                                tmp.add(cnt + ":" + (i + 1));
                                map.put(elem, tmp);
                            }
                        }
                        cnt++;
                    } catch (IOException e) {
                        System.out.println("Wrong data!");
                    }
                }
            } catch (FileNotFoundException e) {
                System.out.println("Input file doesn't exist!");
            } catch (IOException e) {
                System.out.println("Wrong input data!");
            }

            try (BufferedWriter out = new BufferedWriter(
                    new OutputStreamWriter(new FileOutputStream(args[1]), StandardCharsets.UTF_8))) {
                for (Map.Entry<String, ArrayList<String>> entry : map.entrySet()) {
                    out.write(entry.getKey() + " " + entry.getValue().size());
                    for (String strTemp : entry.getValue()) {
                        out.write(" " + strTemp);
                    }
                    out.write("\n");
                }
            } catch (FileNotFoundException e) {
                System.out.println("Can`t create an output file");
            } catch (IOException e) {
                System.out.println("Wrong output data!");
            }
        } else {
            System.out.println("Wrong input data");
        }
    }
}
