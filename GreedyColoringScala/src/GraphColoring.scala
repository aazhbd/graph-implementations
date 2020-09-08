// blank line

/** Implementation of greedy graph coloring in Scala
 *
 * The code produces colored graphs from input adjacency
 * lists from files or manually created objects.
 *
 * @author Abdullah Al Zakir Hossain, aazhbd@conveylive.com
 * @since August 2020
 */

import scala.collection.mutable
import scala.annotation.tailrec
import scala.io.Source

object GraphColoring {

    def main(args: Array[String]) {
        val gc = new Graph(List(1, 2, 3, 4, 5, 6))

        gc.addConnection(1, 2)
        gc.addConnection(1, 3)
        gc.addConnection(2, 4)
        gc.addConnection(2, 5)
        gc.addConnection(3, 5)
        gc.addConnection(4, 1)
        gc.addConnection(5, 2)
        gc.addConnection(5, 4)
        gc.addConnection(6, 3)
        gc.addConnection(6, 4)

        val result: mutable.Map[Int, Int] = applyColors(
            gc.graph(),
            gc.vertices)

        for ((k, v) <- result)
            printf("vertex: %s has color: %s\n", k, v)

        println("\nGraph from file\n")
        /**
         * Test cases from files. The string with file name needs to be
         * updated for different graphs. The file should contain a line
         * of ordering values and adjacency list of the graph.
         */
        val infile = Source.fromFile("../GraphInputs/circular1.txt")
        val lines = infile.getLines().toList

        /** creating the graph object to color it with first line of input file. */
        val colorGraph = new Graph(
            lines
                .head
                .toString
                .split("\\s+")
                .map(_.toInt).toList)

        /** Adding the edges to the graph object. */
        for (c <- lines.tail) {
            val ed = c.toString
                .split("\\s+")
                .map(_.toInt).toList
            colorGraph.addConnection(ed.head, ed(1))
        }

        /** Applying the colors to the graph. */
        val colors: mutable.Map[Int, Int] = applyColors(
            colorGraph.graph(),
            colorGraph.vertices)

        /** printing the colored graph with formatting. */
        for ((k, v) <- colors)
            printf("vertex: %s has color: %s\n", k, v)

        infile.close()
    }

    /** The class to contain the adjacency list information
     *
     * @param vertices graph vertex ordering.
     */
    class Graph(val vertices: List[Int]) {
        private var _graph: mutable.Map[Int, List[Int]] = mutable.Map[Int, List[Int]]()

        def graph(): mutable.Map[Int, List[Int]] = _graph

        def graph(g: mutable.Map[Int, List[Int]]): Unit = {
            _graph = g
        }

        this.vertices.foreach(this.graph += _ -> List())

        def addConnection(s: Int, t: Int): Unit = {
            if (!vertices.contains(s) || !vertices.contains(t)) return
            this.graph += s -> (this.graph()(s) :+ t)
            this.graph += t -> (this.graph()(t) :+ s)
        }
    }

    /** Finding the smallest available color.
     *
     * @param n    Initial color
     * @param used Already used colors
     * @return
     */
    @tailrec
    private def colorPicker(n: Int, used: Set[Int]): Int =
        if (!used.contains(n)) n else colorPicker(n + 1, used)

    /** Applies the colors on vertices
     *
     * @param graph    graph edges
     * @param vertices vertex ordering
     * @return
     */
    def applyColors(graph: mutable.Map[Int,
        List[Int]], vertices: List[Int]): mutable.Map[Int, Int] = {
        val coloredGraph = mutable.Map[Int, Int]()

        def getUsedColors(vertex: Int): Set[Int] = {
            val usedColors: Set[Int] = graph(vertex).collect {
                case e if coloredGraph.contains(e) => coloredGraph(e)
            }.toSet
            usedColors
        }

        vertices.map(vertex => {
            coloredGraph.put(vertex, colorPicker(0, getUsedColors(vertex)))
        })
        coloredGraph
    }

}
