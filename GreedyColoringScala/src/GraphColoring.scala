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

        val result = gc.applyColorsFunc()
        for ((k, v) <- result) printf("vertex: %s, color: %s\n", k, v)

        // test cases from files.

        println("\nGraph from file\n")

        val infile = Source.fromFile("/home/expressions/Downloads/in.txt")
        val lines = infile.getLines().toList
        val vs = lines.head.toString.split("\\s+").map(_.toInt).toList
        val colorGraph = new Graph(vs)

        for(c <- lines.tail) {
            val ed = c.toString.split("\\s+").map(_.toInt).toList
            colorGraph.addConnection(ed.head, ed(1))
        }

        val colors = colorGraph.applyColorsFunc()
        for ((k, v) <- colors) printf("vertex: %s, color: %s\n", k, v)

        infile.close()
    }

    class Graph(val vertices: List[Int]) {
        private var _graph: mutable.Map[Int, List[Int]] = mutable.Map[Int, List[Int]]()

        def graph(): mutable.Map[Int, List[Int]] = _graph

        def graph(g: mutable.Map[Int, List[Int]]): Unit = {
            _graph = g
        }

        this.vertices.foreach(this.graph += _ -> List())

        @tailrec
        private def colorPicker(n: Int, used: List[Int]): Int = if (!used.contains(n)) n else colorPicker(n + 1, used)

        def addConnection(s: Int, t: Int): Unit = {
            if (!vertices.contains(s)) return
            if (!vertices.contains(t)) return

            val g = this.graph()
            g += s -> (this.graph()(s) :+ t)
            this.graph(g)
        }

        def applyColorsFunc(): mutable.Map[Int, Int] = {
            val colored = mutable.Map[Int, Int]()

            def getUsedColors(vertex: Int): List[Int] = {
                val usedColors = this.graph()(vertex).collect {
                    case e if colored.contains(e) => colored(e)
                }
                usedColors
            }

            this.vertices.map(vertex => {
                colored.put(vertex, colorPicker(0, getUsedColors(vertex)))
            })
            colored
        }
    }

}

//def getc(vertex: Int): Int = {
//    used_colors :+ this.graph()(vertex).filter(e => colored.contains(e)).map(e => colored(e))
//    (colorPicker(-1, used_colors))
//}
//
//
//def applyColors(): mutable.Map[Int, Int] = {
//    val colored = mutable.Map[Int, Int]()
//    var used_colors = List[Int]()
//
//    def getColor(vertex: Int): Int = {
//        this.graph()(vertex).foreach(e => {
//            if (colored.contains(e)) {
//                used_colors = used_colors :+ colored(e)
//            }
//        })
//        (colorPicker(0, used_colors))
//    }
//
//    this.vertices.foreach(vertex => colored.put(vertex, getColor(vertex)))
//    colored
//}
//
//def applyColorsImper(): mutable.Map[Int, Int] = {
//    var colored = mutable.Map[Int, Int]()
//    var used_colors = List[Int]()
//
//    for (vertex <- this.vertices) {
//        var edges = this.graph()(vertex);
//
//        for (e <- edges) {
//            if (colored.contains(e)) {
//                used_colors = used_colors :+ colored(e)
//            }
//        }
//
//        var color = (colorPicker(0, used_colors))
//        colored.put(vertex, color)
//    }
//    colored
//}

