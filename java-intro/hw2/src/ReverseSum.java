import java.io.IOException;
import java.util.ArrayList;

public class ReverseSum {
    public static void main(String[] args) {
        try {
            MyScanner scan = new MyScanner();
            //Scanner scan = new Scanner(System.in);
            ArrayList<ArrayList<Integer>> a = new ArrayList<ArrayList<Integer>>();

            while (scan.hasNext()) {
                a.add(scan.nextArray());
            }

            int[] col = new int[scan.mxlength];
            int[] str = new int[a.size()];

            for (int i = 0; i < a.size(); i++) {
                for (int j = 0; j < a.get(i).size(); j++) {
                    col[j] += a.get(i).get(j);
                    str[i] += a.get(i).get(j);
                }
            }

            for (int i = 0; i < a.size(); i++) {
                for (int j = 0; j < a.get(i).size(); j++) {
                    System.out.print(str[i] + col[j] - a.get(i).get(j) + " ");
                }
                System.out.println();
            }

        } catch (NumberFormatException e) {
            System.out.println("Found not an integer number!");
            return;
        } catch (ArrayIndexOutOfBoundsException e) {
            System.out.println("Wrong array index!");
            return;
        } catch (IOException e) {
            System.out.println("Wrong input!");
        }

    }
}
