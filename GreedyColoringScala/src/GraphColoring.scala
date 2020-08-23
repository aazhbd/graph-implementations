import scala.collection.mutable.Map

object GraphColoring {

    def main(args: Array[String]) {
        var gc = new Graph(List(1, 2, 3, 4, 5, 6))

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

        var result = gc.applyColors()
        for ((k, v) <- result) printf("vertex: %s, color: %s\n", k, v)
    }

    class Graph(val vertices: List[Int]) {
        private var _graph = Map[Int, List[Int]]()

        this.vertices.foreach(this.graph += _ -> List())

        def graph() = _graph

        def graph(g: Map[Int, List[Int]]) = {
            _graph = g
        }

        def addConnection(s: Int, t: Int): Unit = {
            if (!vertices.contains(s)) return
            if (!vertices.contains(t)) return

            val g = this.graph()

            def getConn(s: Int, t: Int) = {
                (s -> (this.graph()(s) :+ t))
            }

            g += getConn(s, t)
            this.graph(g)
        }

        def colorPicker(n: Int, used: List[Int]): Int = {
            if (used.contains(n)) colorPicker(n + 1, used)
            else n
        }

        def applyColorsImper(): Map[Int, Int] = {
            var colored = Map[Int, Int]()
            var used_colors = List(-1)

            for (vertex <- this.vertices) {
                var edges = this.graph()(vertex);

                for (e <- edges) {
                    if (colored.contains(e)) {
                        used_colors = used_colors :+ colored(e)
                    }
                }

                var color = (colorPicker(-1, used_colors))
                colored.put(vertex, color)
            }
            colored
        }

        def applyColors(): Map[Int, Int] = {
            val colored = Map[Int, Int]()
            var used_colors = List(-1)

            def getColor(vertex: Int): Int = {
                this.graph()(vertex).foreach(e => {
                    if (colored.contains(e)) {
                        used_colors = used_colors :+ colored(e)
                    }
                })
                (colorPicker(-1, used_colors))
            }

            this.vertices.foreach(vertex => colored.put(vertex, getColor(vertex)))
            colored
        }
    }

}

//def getc(vertex: Int): Int = {
//    used_colors :+ this.graph()(vertex).filter(e => colored.contains(e)).map(e => colored(e))
//    (colorPicker(-1, used_colors))
//}