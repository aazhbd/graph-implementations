import scala.collection.mutable.Map

object GraphColoring {

    def main(args : Array[String]) {
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

        println(gc.colorPicker(0, List(1, 2, 3, 4)))

        println(gc.graph())
    }

    class Graph(val vertices: List[Int]) {
        private var _graph = Map[Int, List[Int]]()

        this.vertices.foreach(this.graph += _ -> List())

        def graph() = _graph
        def graph(g: Map[Int, List[Int]]) = { _graph = g }

        def addConnection(s:Int, t:Int): Unit = {
            if (!vertices.contains(s)) return
            if (!vertices.contains(t)) return

            val g = this.graph()
            def getConn(s: Int, t:Int) = {
                (s -> (this.graph()(s) :+ t))
            }
            g += getConn(s, t)
            this.graph(g)
        }

        def colorPicker(n: Int, used:List[Int]): Int = {
            if(used.contains(n)) colorPicker(n + 1, used)
            else n
        }

        def applyColors(): Unit = {
            println(this.graph())
        }
    }

}
