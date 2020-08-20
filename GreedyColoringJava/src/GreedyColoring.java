import java.util.*;
import java.io.*;

public class GreedyColoring {
    public static final String input = "in.txt";
    private HashMap<Integer, List<Integer>> graph;
    private List<Integer> vertices;

    public GreedyColoring(Integer v[]) {
        this.vertices = Arrays.asList(v);

        this.graph = new HashMap<Integer, List<Integer>>();

        for(int vertex : this.vertices) {
            this.graph.put(vertex, new ArrayList<Integer>());
        }
    }

    public static void main(String args[]) {
        FileInputStream instream = null;

    }
}
