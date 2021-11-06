import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Vector;
import java.util.HashMap;
import java.util.Map;
import java.util.Comparator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

class Ticket
{
    public HashMap<String, Vector<Integer>> validValues;
    public Vector<Integer> myTicket;
    public Vector<Vector<Integer>> nearbyTickets;
    public int numFields;

    public Ticket(String filename) throws IOException
    {
        Path filePath = Path.of(filename);
        String content = Files.readString(filePath);

        // Extract the 3 sections in the file
        Matcher m;
        Pattern reSections = Pattern.compile("(.*?)\\s+your ticket:\\s+(.*?)\\s+nearby tickets:\\s+(.*)", Pattern.DOTALL);
        m = reSections.matcher(content);
        if (!m.find()) {
            throw new IOException("Could not match file format");
        }

        String section1 = m.group(1);
        String section2 = m.group(2);
        String section3 = m.group(3);

        // Section 1: Ranges
        validValues = new HashMap<String, Vector<Integer>>();
        Pattern reRanges = Pattern.compile("(.*?):\\s+(\\d+)-(\\d+)\\s+or\\s+(\\d+)-(\\d+)");
        m = reRanges.matcher(section1);
        while (m.find()) {
            String name = m.group(1);
            int start, stop;

            validValues.put(name, new Vector<Integer>());

            start = Integer.parseInt(m.group(2));
            stop = Integer.parseInt(m.group(3));

            for (int i = start; i <= stop; i++) {
                validValues.get(name).add(i);
            }

            start = Integer.parseInt(m.group(4));
            stop = Integer.parseInt(m.group(5));

            for (int i = start; i <= stop; i++) {
                validValues.get(name).add(i);
            }
        }

        // Section 2: My ticket
        myTicket = new Vector<Integer>();
        String[] strArr;
        strArr = section2.split(",");
        numFields = strArr.length;

        for (int i = 0; i < strArr.length; i++) {
            myTicket.add(Integer.parseInt(strArr[i]));
        }

        // Section 3: Nearby tickest
        nearbyTickets = new Vector<Vector<Integer>>();
        String[] lineArr = section3.split("\n");

        for (int i = 0; i < lineArr.length; i++) {
            nearbyTickets.add(new Vector<Integer>());
            strArr = lineArr[i].split(",");

            for (int j = 0; j < strArr.length; j++) {
                nearbyTickets.get(i).add(Integer.parseInt(strArr[j]));
            }
        }
    }

    public void findTicketScanningErrorRate()
    {
        int err = 0;
        Vector<Integer> invalidTickets = new Vector<Integer>();

        for (int i = 0; i < nearbyTickets.size(); i++) {
            boolean invalid = false;
            Vector<Integer> ticket = nearbyTickets.get(i);

            for (int j = 0; j < ticket.size(); j++) {
                int field = ticket.get(j);
                boolean found = false;

                for (Map.Entry<String, Vector<Integer>> entry: validValues.entrySet()) {
                    String k = entry.getKey();
                    Vector<Integer> v = entry.getValue();

                    if (v.contains(field)) {
                        found = true;
                        break;
                    }
                }

                if (!found) {
                    // System.out.printf("Error in ticket %d (field = %d)\n", i, field);
                    err += field;

                    // Mark this to be discarded
                    invalid = true;
                }
            }

            if (invalid) {
                System.out.printf("Ticket %d is invalid\n", i);
                invalidTickets.add(i);
            }
        }

        // Remove invalid tickets from the nearby ticket list
        invalidTickets.stream()
             .sorted(Comparator.reverseOrder())
             .forEach(i->nearbyTickets.remove(i.intValue()));

        System.out.printf("Ticket scanning error rate: %d\n", err);
    }


    public void findFieldOrder()
    {
        // Add my own ticket to the bunch
        nearbyTickets.add(myTicket);

        // Find all the possible columns for each field name
        HashMap<String, Vector<Integer>> candidates = new HashMap<String, Vector<Integer>>();

        for (Map.Entry<String, Vector<Integer>> entry: validValues.entrySet()) {
            String k = entry.getKey();
            Vector<Integer> v = entry.getValue();

            candidates.put(k, new Vector<Integer>());

            // Check each column and check that all cells are within the valid range
            for (int i = 0; i < numFields; i++) {
                boolean isValid = true;
                for (int j = 0; j < nearbyTickets.size(); j++) {
                    int field = nearbyTickets.get(j).get(i);
                    // System.out.printf("    field = %d\n", field);
                    if (!v.contains(field)) {
                        // System.out.println("    INVALID");
                        isValid = false;
                        break;
                    }
                }
                if (isValid) {
                    candidates.get(k).add(i);
                }
            }
        }


        // Isolate the candidates - some have only one, so they are certain
        HashMap<String, Integer> fieldColumn = new HashMap<String, Integer>();

        int n = 0;
        while (n < numFields) {
            // System.out.println("Field indexes:");
            // candidates.forEach((k, v) -> {
            //     System.out.printf("    %s: ", k);
            //     for (int i = 0; i < v.size(); i++) {
            //         System.out.printf("%d ", v.get(i));
            //     }
            //     System.out.println();
            // });

            n = 0;

            for (Map.Entry<String, Vector<Integer>> entry: candidates.entrySet()) {
                String k = entry.getKey();
                Vector<Integer> v = entry.getValue();

                // If a field name only has one candidate, save this and remove
                // it from all other candidate lists

                if (v.size() == 1) {
                    int field = v.get(0);
                    fieldColumn.put(k, field);

                    for (Map.Entry<String, Vector<Integer>> subEntry: candidates.entrySet()) {
                        subEntry.getValue().removeElement(field);
                    }
                    break;
                }

                n += 1;
            }
        }

        System.out.println("Field indexes:");
        fieldColumn.forEach((k, v) -> {
            System.out.printf("    %s: %d\n", k, v);
        });


        // Multiply all fields on my ticket that starts with "departure".
        long product = 1;
        for (Map.Entry<String, Integer> entry: fieldColumn.entrySet()) {
            if (entry.getKey().startsWith("departure")) {
                int i = entry.getValue();
                int value = myTicket.get(i);
                System.out.printf("* value = %d\n", value);
                product *= value;
            }
        }
        System.out.printf("Product of my ticket fields starting with \"departure\": %d\n", product);
    }

    public void show()
    {
        System.out.println("Ranges:");
        validValues.forEach((k, v) -> {
            System.out.printf("    %s: ", k);
            for (int i = 0; i < v.size(); i++) {
                System.out.printf("%d ", v.get(i));
            }
            System.out.println();
        });

        System.out.println("My ticket:");
        System.out.printf("    ");
        for (int i = 0; i < myTicket.size(); i++) {
            System.out.printf("%d ", myTicket.get(i));
        }
        System.out.println();

        System.out.println("Nearby tickets:");
        for (int i = 0; i < nearbyTickets.size(); i++) {
            System.out.printf("    ");
            for (int j = 0; j < nearbyTickets.get(i).size(); j++) {
                System.out.printf("%d ", nearbyTickets.get(i).get(j));
            }
            System.out.println();
        }

        System.out.printf("Number of fields: %d\n", numFields);
        System.out.println();
    }
}

class Main
{  
    public static void main(String args[]) throws IOException
    {
        int i;
        // Ticket t = new Ticket("test_input1.txt");
        // Ticket t = new Ticket("test_input2.txt");
        Ticket t = new Ticket("puzzle_input.txt");

        // t.show();
        t.findTicketScanningErrorRate();
        t.findFieldOrder();
        // t.show();
    }  
}
