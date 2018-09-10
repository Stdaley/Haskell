import java.io.*;

public class Bugs2 {
    public static void main(String argv []) {
        int i = 7;
        int j = 4;

        int k = (int) ( (i-j) * (j-i) * Math.exp(j/i)) ;
 
        flyRocket(5);
        flyRocket(k);
    }
    
    public static void flyRocket(int n) {
        System.out.println("Countdown starting at " + n);
        countDown(n);
        System.out.println("I'm a rocket man, " +
                           "burning up his fuse out here alone.");
        System.out.println("Beginning Re-Entry.");
        reEntry(n);
    }

    public static void countDown(int n) {
        // Run a countdown until n reaches 0 ***or below*** (Hint!).
        if (n <= 0) // Added a less than & equal to sign to properly count down until it reaches 0.
        {  
          System.out.println("Lift off!");
        }
        else
        {
            System.out.println(n + "...");
            countDown(n-1);
        }
    }
// PART 3
    public static void reEntry(int n) {
        // Fill in code using a while or for loop to count
        // from 0 back up to n, printing each number, and 
        // then announce a landing.

     for (int i = 0; i <= n; i++) // Created a for-loop that starts at 0 and increases until it reached n
     {
       System.out.println(i + "..."); //Prints each number
     }
     System.out.println("You are about to land!"); // Landing message before final touchdown

 System.out.println("Touchdown!");
    }
}
