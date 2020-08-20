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

    public void addConnection(Integer f, Integer t) {
        if(this.vertices.contains(f) && this.vertices.contains(t))
        {
            List<Integer> edges = this.graph.get(f);

            if(!edges.contains(t)) {
                edges.add(t);
            }

            this.graph.put(f, edges);
        }
    }

    public int pickColor(List<Integer> colors) {
        for(int c = 0; ; c++) {
            if(!colors.contains(c)) {
                return c;
            }
        }
    }

    public HashMap<Integer, Integer> applyColors() {
        HashMap<Integer, Integer> colored = new HashMap<Integer, Integer>();
        List<Integer> used_colors = new ArrayList<Integer>();

        for(int vertex : this.vertices) {
            List<Integer> edges = this.graph.get(vertex);

            for(int e : edges) {
                if(colored.containsKey(e)) {
                    used_colors.add(colored.get(e));
                }
            }

            colored.put(vertex, pickColor(used_colors));
        }

        return colored;
    }

    public static void main(String args[]) {
        FileInputStream instream = null;

    }
}
