import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Vector;
import java.util.HashMap;
import java.time.Instant;

class MemoryGame
{
    int[] startingNumbers;

    public MemoryGame(int[] myStartingNumbers)
    {
        startingNumbers = myStartingNumbers;
    }

    public static int getPreviousOccurrenceOfLastElement(Vector<Integer> v)
    {
        int l = v.lastElement(); 

        // Search vector in reverse
        for (int i = v.size()-2; i >= 0; i--) {
            if (v.get(i) == l) {
                return i;
            }
        }

        // Not found
        return -1;
    }

    public void playNaive(int target)
    {
        int turn = 0;
        Vector<Integer> spokenNumbers = new Vector<Integer>();

        while (true) {
            int n;

            if (turn < startingNumbers.length) {
                n = startingNumbers[turn];
            }
            else {
                int i = getPreviousOccurrenceOfLastElement(spokenNumbers);
                if (i < 0) {
                    // No previous occurrence found
                    n = 0;
                }
                else {
                    // The number was spoken before
                    n = (turn-1) - i;
                }
            }

            spokenNumbers.add(n);
            turn += 1;

            if (turn >= target) {
                System.out.printf("Turn %d: %d\n", turn, n);
                break;
            }
        }
    }

    public void play(int target)
    {
        int turn = 0;
        int lastSpokenNumber = -1;
        HashMap<Integer, Integer> spokenNumberLatestIndex = new HashMap<Integer, Integer>();

        while (true) {
            int n;

            if (turn < startingNumbers.length) {
                n = startingNumbers[turn];
            }
            else {
                if (spokenNumberLatestIndex.containsKey(lastSpokenNumber)) {
                    int l = spokenNumberLatestIndex.get(lastSpokenNumber);
                    n = (turn-1) - l;
                }
                else {
                    n = 0;
                }
            }

            if ((turn % 1000000) == 0) {
                System.out.printf("Turn %d: %d\n", turn, n);
            }

            spokenNumberLatestIndex.put(lastSpokenNumber, turn-1);
            lastSpokenNumber = n;
            turn += 1;

            if (turn >= target) {
                System.out.printf("Final turn %d: %d\n", turn, n);
                break;
            }
        }
    }
}

class Main
{  
    static int[] getInput(String filename) throws IOException
    {
        int i;
        Path filePath = Path.of(filename);
        String content = Files.readString(filePath);
        String[] strArr = content.trim().split(",");
        int[] intArr = new int[strArr.length];
        for (i = 0; i < strArr.length; i++) {
            intArr[i] = Integer.parseInt(strArr[i]);
        }

        return intArr;
    }

    public static void main(String args[]) throws IOException
    {  
        int i;
        // int[] startingNumbers = getInput("test_input1.txt");
        // int[] startingNumbers = getInput("test_input2.txt");
        // int[] startingNumbers = getInput("test_input3.txt");
        // int[] startingNumbers = getInput("test_input4.txt");
        // int[] startingNumbers = getInput("test_input5.txt");
        // int[] startingNumbers = getInput("test_input6.txt");
        // int[] startingNumbers = getInput("test_input7.txt");
        int[] startingNumbers = getInput("puzzle_input.txt");

        MemoryGame game = new MemoryGame(startingNumbers);

        // game.play(2020);
        game.play(30000000);
    }  
}
