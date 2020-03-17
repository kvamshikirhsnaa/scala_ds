package part1

object SierpinskiTriangles extends App {

  def seirpinski(n: Int): String = {

    def seirpinskiTailrec(level: Int, acc: List[String]): List[String] = {
      if (level == n) acc
      else {
        val spaces = " " * (1 << level)
        println(spaces)

        val topTriangles = acc.map(x => spaces + x + spaces )
        println("TOP Triangle")
        println("------------")
        println(topTriangles)
        println()

        val bottomTriangles = acc.map(x => x + " " + x)
        println("Bottom Triangles")
        println("----------------")
        println(bottomTriangles)
        println()

        val newTriangles = topTriangles ++ bottomTriangles
        println("new Triangles")
        println("-------------")
        println(newTriangles)
        println()

        seirpinskiTailrec(level + 1, newTriangles)
      }
    }
    seirpinskiTailrec(0, List("*")).mkString("\n")
  }

  println(seirpinski(3))

}
