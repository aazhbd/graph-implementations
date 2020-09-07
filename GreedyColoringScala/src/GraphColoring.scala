// blank line

/** Implementation of greedy graph coloring in Scala
 *
 * The code produces colored graphs from input adjacency
 * lists from file or manually created objects.
 * @author Abdullah Al Zakir Hossain, August 2020
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
         * updated for different graph. The file should contain a line
         * of ordering values and adjacency list of the graph.
         */
        val infile = Source.fromFile("/home/abdullah/Documents/GraphInputs/completegraph.txt")
        val lines = infile.getLines().toList

        /** creating the graph object to color it with first line of input file. */
        val colorGraph = new Graph(
            lines
                .head
                .toString
                .split("\\s+")
                .map(_.toInt).toList)

        for (c <- lines.tail) {
            val ed = c.toString
                .split("\\s+")
                .map(_.toInt).toList
            colorGraph.addConnection(ed.head, ed(1))
        }

        val colors: mutable.Map[Int, Int] = applyColors(
            colorGraph.graph(),
            colorGraph.vertices)

        for ((k, v) <- colors)
            printf("vertex: %s has color: %s\n", k, v)

        infile.close()
    }

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

    @tailrec
    private def colorPicker(n: Int, used: Set[Int]): Int =
        if (!used.contains(n)) n else colorPicker(n + 1, used)

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
