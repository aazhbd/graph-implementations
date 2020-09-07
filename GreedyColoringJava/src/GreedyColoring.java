// blank line

/**
 * Implementation of greedy graph coloring in Java
 * <p>
 * The code produces colored graphs from input adjacency
 * lists from files or manually created objects.
 *
 * @author Abdullah Al Zakir Hossain, August 2020
 */

import java.util.*;
import java.io.*;

public class GreedyColoring {
    private HashMap<Integer, List<Integer>> graph;
    private List<Integer> vertices;

    /**
     * Initializes the graph vertex ordering and empty edges.
     * @param v vertex ordering.
     */
    public GreedyColoring(List<Integer> v) {
        this.vertices = v;
        this.graph = new HashMap<Integer, List<Integer>>();

        for (int vertex : this.vertices) {
            this.graph.put(vertex, new ArrayList<Integer>());
        }
    }

    /**
     * Adds connection to the graph
     * @param f vertex number
     * @param t vertex number
     */
    public void addConnection(Integer f, Integer t) {
        if (!this.vertices.contains(f) || !this.vertices.contains(t)) return;

        List<Integer> fedges = this.graph.get(f);
        if (!fedges.contains(t)) fedges.add(t);
        this.graph.put(f, fedges);

        List<Integer> tedges = this.graph.get(t);
        if (!tedges.contains(f)) tedges.add(f);
        this.graph.put(t, tedges);
    }

    /**
     * Finding the smallest available color.
     * @param colors
     * @return
     */
    public int pickColor(HashSet<Integer> colors) {
        for (int c = 0; ; c++) {
            if (!colors.contains(c)) {
                return c;
            }
        }
    }

    /**
     * Applies the colors on vertices
     * @return the colored graph information.
     */
    public HashMap<Integer, Integer> applyColors() {
        HashMap<Integer, Integer> colored = new HashMap<Integer, Integer>();

        for (int vertex : this.vertices) {
            List<Integer> edges = this.graph.get(vertex);
            HashSet<Integer> used_colors = new HashSet<Integer>();

            for (int e : edges) {
                if (colored.containsKey(e)) {
                    used_colors.add(colored.get(e));
                }
            }
            colored.put(vertex, pickColor(used_colors));
        }
        return colored;
    }

    /**
     *  prints the graph values without coloring information.
     */
    public void printGraph() {
        for (int vertex : this.vertices) {
            List<Integer> edges = this.graph.get(vertex);
            System.out.println(vertex + " has connection with "
                    + Arrays.toString(edges.toArray()));
        }
    }

    /**
     * Prints the coloring information of the graph.
     * @param colored color information of the graph
     */
    public void printColors(HashMap<Integer, Integer> colored) {
        Set<Map.Entry<Integer, Integer>> entries = colored.entrySet();

        for (Map.Entry<Integer, Integer> entry : entries) {
            System.out.println("vertex: " + entry.getKey()
                    + " has color: " + entry.getValue());
        }
    }

    public static void main(String[] args) {
        /**
         * Test cases from files. The string with file name needs to be
         * updated for different graphs. The file should contain a line
         * of ordering values and adjacency list of the graph.
         */
        String input = "/home/abdullah/Documents/GraphInputs/completegraph.txt";
        FileInputStream instr;

        try {
            instr = new FileInputStream(input);
            System.setIn(instr);
        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
        }

        Scanner in = new Scanner(System.in);

        /** creating the graph object to color it with first line of input file. */
        int[] vs = Arrays.stream(in.nextLine()
                .split("\\s+"))
                .mapToInt(Integer::parseInt)
                .toArray();

        Integer[] Vs = Arrays.stream(vs)
                .boxed().toArray(Integer[]::new);

        GreedyColoring graph = new GreedyColoring(Arrays.asList(Vs));

        /**
         * Adding the edges to the graph object.
         */
        while (in.hasNext()) {
            int[] eg = Arrays.stream(in.nextLine()
                    .split("\\s+"))
                    .mapToInt(Integer::parseInt).toArray();

            graph.addConnection(eg[0], eg[1]);
        }

        graph.printGraph();

        /**
         * Applying the colors to the graph and printing with formatting.
         */
        graph.printColors(graph.applyColors());

        GreedyColoring gc = new GreedyColoring(
                Arrays.asList(6, 5, 4, 3, 2, 1));

        gc.addConnection(1, 2);
        gc.addConnection(1, 3);
        gc.addConnection(2, 4);
        gc.addConnection(2, 5);
        gc.addConnection(3, 5);
        gc.addConnection(4, 1);
        gc.addConnection(5, 2);
        gc.addConnection(5, 4);
        gc.addConnection(6, 3);
        gc.addConnection(6, 4);

        gc.printGraph();

        gc.printColors(gc.applyColors());
    }
}

