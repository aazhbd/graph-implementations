import java.util.*;
import java.io.*;

public class GreedyColoring {
    private HashMap<Integer, List<Integer>> graph;
    private List<Integer> vertices;

    public GreedyColoring(List<Integer> v) {
        this.vertices = v;
        this.graph = new HashMap<Integer, List<Integer>>();

        for (int vertex : this.vertices) {
            this.graph.put(vertex, new ArrayList<Integer>());
        }
    }

    public void addConnection(Integer f, Integer t) {
        if (!this.vertices.contains(f) || !this.vertices.contains(t)) return;

        List<Integer> fedges = this.graph.get(f);
        if (!fedges.contains(t)) fedges.add(t);
        this.graph.put(f, fedges);

        List<Integer> tedges = this.graph.get(t);
        if (!tedges.contains(f)) tedges.add(f);
        this.graph.put(t, tedges);
    }

    public int pickColor(HashSet<Integer> colors) {
        for (int c = 0; ; c++) {
            if (!colors.contains(c)) {
                return c;
            }
        }
    }

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

    public void printGraph() {
        for (int vertex : this.vertices) {
            List<Integer> edges = this.graph.get(vertex);
            System.out.println(vertex + " has connection with " + Arrays.toString(edges.toArray()));
        }
    }

    public void printColors(HashMap<Integer, Integer> colored) {
        Set<Map.Entry<Integer, Integer>> entries = colored.entrySet();

        for (Map.Entry<Integer, Integer> entry : entries) {
            System.out.println("vertex: " + entry.getKey() + " has color: " + entry.getValue());
        }
    }

    public static void main(String[] args) {
        String input = "/home/abdullah/Documents/GraphInputs/completegraph.txt";
        FileInputStream instr = null;

        try {
            instr = new FileInputStream(input);
            System.setIn(instr);
        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
        }

        Scanner in = new Scanner(System.in);

        int[] vs = Arrays.stream(in.nextLine()
                    .split("\\s+"))
                    .mapToInt(Integer::parseInt)
                    .toArray();

        Integer[] Vs = Arrays.stream(vs)
                    .boxed().toArray(Integer[]::new);

        GreedyColoring graph = new GreedyColoring(Arrays.asList(Vs));

        while (in.hasNext()) {
            int[] eg = Arrays.stream(in.nextLine()
                        .split("\\s+"))
                        .mapToInt(Integer::parseInt).toArray();

            graph.addConnection(eg[0], eg[1]);
        }

        graph.printGraph();

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

