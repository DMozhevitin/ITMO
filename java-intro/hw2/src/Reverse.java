import java.io.IOException;
import java.util.ArrayList;
import java.util.Scanner;

public class Reverse {
    public static void main(String[] args) {
        try {
            MyScanner scan = new MyScanner();
            //Scanner scan = new Scanner(System.in);
            ArrayList<ArrayList<Integer>> a = new ArrayList<ArrayList<Integer>>();

            while (scan.hasNext()) {
                String s = scan.nextLine();
                a.add(new ArrayList<Integer>());
                if (s.length() == 0) {
                    continue;
                }
                String[] splitted = s.split("\\s+");
                for (String elem : splitted) {
                    a.get(a.size() - 1).add(Integer.parseInt(elem));
                }
            }

            for (int i = a.size() - 1; i >= 0; i--) {
                for (int j = a.get(i).size() - 1; j >= 0; j--) {
                    System.out.print(a.get(i).get(j) + " ");
                }
                System.out.println();
            }

        } catch (IOException e) {
            System.out.println("Wrong input!");
            return;
        } catch (NumberFormatException e) {
            return;
        } catch (ArrayIndexOutOfBoundsException e) {
            System.out.println("");
            return;
        }

    }

    public static ArrayList<String> mySplit(String s) {
        StringBuilder tmp = new StringBuilder();
        ArrayList<String> list = new ArrayList<String>();
        for (int i = 0; i < s.length(); i++) {
            if (Character.isWhitespace(s.charAt(i)) && tmp.length() > 0) {
                list.add(tmp.toString());
                tmp = new StringBuilder();
            } else {
                tmp.append(s.charAt(i));
            }
        }
        if (tmp.length() > 0)
            list.add(tmp.toString());
        return list;
    }
}
