package search;

import java.util.Scanner;


public class BinarySearchMissing {
    public static void main(String[] args) {
        if (args.length >= 2) {

            int n = args.length - 1, key = Integer.parseInt(args[0]);
            int[] a = new int[n];

            for (int i = 0; i < n; i++) {
                a[i] = Integer.parseInt(args[i + 1]);
            }

            int ans1 = IterativeSearch(a, key), ans2 = RecSearch(a, key, -1, a.length);
            if (ans1 != ans2) {
                System.out.println("Очень жаль, вы проиграли");
            } else {
                System.out.println(ans1);
            }
        } else {
            System.out.println("Очень жаль, вы проиграли");
        }
    }

    //Pre: ∀i a[i] >= a[i + 1] && ∀i a[i] ∈ int && key ∈ int
    static int IterativeSearch(int[] a, int key) { //left-side search
        //Pre
        int left = -1, right = a.length;

        //Inv : (right' = a.length || (right' < a.length && a[right'] <= key) &&
        //&& (left' = -1 || (left' ∈ [0, a.length) && a[left'] = key)) &&
        //&& ((right')' - (left')' <= (right' - left' + 1) / 2) && (right' - left' >= 1)

        while (right - left > 1) {
            //Pre && Inv && (right' - left' > 1)
            int mid = left + (right - left) / 2;
            //Pre && Inv && (right' - left' > 1) && (mid = (right' + left') / 2) && mid ∈ [left', right'])
            if (a[mid] > key) {
                //Pre && Inv && (right' - left' > 1) && (a[mid] > key)
                left = mid;
                //Pre && Inv && ((left')' = mid) && ((right')' = right') && (key < a[(left')']) && (res ∈ [(left')', (right')'])
            } else {
                //Pre && Inv && (right' - left' > 1) && (a[mid] <= key) && (res ∈[(left')', (right')'])
                right = mid;
                //Pre && Inv && ((right')' = mid) && ((left')' = left') && key >= a[(right')'] && res ∈ [(left')', (right')']
            }
        }

        //Pre && Inv && (right' - left' = 1) && (res = right') &&
        //&& ((a[res] = key) || (a[res] - наиболее близкий к ключу элемент массива) || ((right = -1) && (key > a[0]))
        // || ((right = a.length) && (a[a.length] > key))

        if (right != a.length && a[right] == key) {
            //Pre && Inv && (res = a[right']) && (a[right'] = key)
            return right;
        } else {
            //Pre && Inv && (a[right'] - наиболее близкий к ключу элемент массива || (key > a[0]) || (key < a[a.length])
            return (-right - 1);
        }
    }

    //Post: (a' = a) && ((key < a[a.length]) && (res = -(a.length - 1) - 1)) ||
    // || (res < a.length && key >= a[res] && (res = 0 || a[res - 1] > key))


    //Pre: ∀i a[i] >= a[i + 1] && ∀i a[i] ∈ int && key ∈ int &&
    //&& ((right = a.length) || (right ∈ [0, a.length) && key >= a[right]))
    //&& (left = -1) || (key <= a[left] && left ∈ [0, a.length]) && (right' - left' <= (right - left + 1) / 2)  && (right - left >= 1)

    static int RecSearch(int[] a, int key, int left, int right) {
        //Pre
        if (right - left == 1) {
            //Pre && (right - left >= 1) && (key >= a[right])
            if (right != a.length && right != -1 && a[right] != key) {
                //a[right] = key
                return right;
            } else {
                //a[right] - ближайший к ключу элемент массива || key > a[0] || key < a[a.length - 1]
                return (-right - 1);
            }
        } else {
            //Pre && (right - left > 1)
            int mid = left + (right - left) / 2;
            //mid ∈ (left, right) && (mid = (left + right) / 2)
            if (a[mid] > key) {
                //left' = mid && (key < a[left']) && (res ∈ [mid, right])
                return RecSearch(a, key, mid, right);
            } else {
                //right' = mid && (key >= a[right'] && res ∈ [left, mid])
                return RecSearch(a, key, left, mid);
            }
        }
    }

    //Post : (a' = a) && (a[a.length - 1] > key && res = -(a.length - 1) - 1) || (res < a.length && key >= a[res] &&
    // && (res = 0  || a[res - 1] > key))
}

