import java.math.BigInteger;
import java.sql.SQLOutput;
import java.util.Scanner;
import java.util.Stack;

public class Task {
    public static void main(String[] args) {
        Scanner scan = new Scanner(System.in);
        String s = scan.nextLine();
        String[] splitted = s.split("\\s+");

        Stack<Integer> st = new Stack<Integer>();
        for (String elem : splitted) {
            try {
                int n = Integer.parseInt(elem);
                st.push(n);
            } catch (NumberFormatException e) {
                int a = st.peek();
                st.pop();

                int b = st.peek();
                st.pop();

                if (elem.equals("+")) {
                    st.push(a + b);
                } else if (elem.equals("-")) {
                    st.push(b - a);
                } else if (elem.equals("*")) {
                    st.push(a * b);
                }
            }
        }

        System.out.println(st.peek());
    }
}