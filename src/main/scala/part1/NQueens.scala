package part1

object NQueens extends App {

  def nQueens(n: Int): List[String] = {

    def conflict(position: Int, queens: List[Int]): Boolean = {
      def conflictOneQueen(position: Int, queen: Int, index: Int): Boolean = {
        position == queen || index + 1 == (queen - position) || index + 1 == (position - queen)
      }

      queens.zipWithIndex.exists( pair => {
        val (queen, index) = pair
        conflictOneQueen( position, queen, index )
      } )
    }

    /*
       n = 4
       nqt(0, [], [])
       nqt(0, [0], [])
       nqt(1, [0], [])
       nqt(2, [0], [])
       nqt(0, [2,0], [])
       nqt(1, [2,0], [])
       nqt(2, [2,0], [])
       nqt(3, [2,0], [])
       nqt(4, [2,0], [])
       nqt(3, [0], [])
       nqt(0, [3,0], [])
       nqt(1, [3,0], [])
       nqt(2, [3,0],[])
       nqt(3, [3,0],[])
       nqt(4, [3,0], [])
       nqt(4, [0], [])
       nqt(1, [], [])
       nqt(0, [1], [])
       nqt(1, [1], [])
       nqt(2, [1], [])
       nqt(3, [1], [])
       nqt(0, [3,1], [])
       nqt(0, [0,3,1], [])
       nqt(1, [0,3,1], [])
       nqt(2, [0,3,1], [])   //  if (currQueens.length == n - 1) {
        newSolutions = [2,0,3,1]

      nqt(3, [0,3,1],[2,0,3,1])
      nqt(4, [0,3,1], [2,0,3,1])   // (currPosition >= n)
      nqt(1, [3,1],[2,0,3,1])    // conflict
      nqt(2,[3,1],[2,0,3,1])
      nqt(3, [3,1], [2,0,3,1])
      nqt(4,[3,1],[2,0,3,1])     // (currPosition >= n)
      nqt(4, [1], [2,0,3,1])    // (currPosition >= n)
      nqt(2, [], [2,0,3,1])
      nqt(0, [2], [2,0,3,1])
      nqt(0,[0,2],[2,0,3,1])
      nqt(1, [0,2],[2,0,3,1])
      nqt(2, [0,2], [2,0,3,1])
      nqt(3, [0,2],[2,0,3,1])
      nqt(0, [3,0,2], [2,0,3,1])
      nqt(1, [3,0,2], [2,0,3,1])  // (currQueens.length == n - 1)
       newSolution = [1,3,0,2]

      nqt(2, [3,0,2], [[2,0,3,1], [1,3,0,2]])
      nqt(3,[3,0,2], [[2,0,3,1], [1,3,0,2]])
      nqt(4, [3,0,2], [[2,0,3,1], [1,3,0,2]])
      nqt(4, [0,2], [[2,0,3,1], [1,3,0,2]])
      nqt(1, [2], [[2,0,3,1], [1,3,0,2]])
      .......................... .....  // checks all possibilities but for n = 4 only these 2 possibles

      [[2,0,3,1], [1,3,0,2]]

     */

    def nQueensTailrec(currPosition: Int, currQueens: List[Int], solutions: List[List[Int]]): List[List[Int]] = {
      if (currPosition >= n && currQueens.isEmpty) solutions
      else if (currPosition >= n) {
        nQueensTailrec( currQueens.head + 1, currQueens.tail, solutions )
      }
      else if (conflict( currPosition, currQueens )) {
        nQueensTailrec( currPosition + 1, currQueens, solutions )
      }
      else if (currQueens.length == n - 1) {
        val newSolution = currPosition :: currQueens
        // println( newSolution )
        nQueensTailrec( currPosition + 1, currQueens, newSolution :: solutions )
      }
      else {
        nQueensTailrec( 0, currPosition :: currQueens, solutions )
      }
    }

    /*
       val top = (1 to n).map(_ => "_").mkString(".",".",".")
       top: String = ._._._._.

       val bottom = (0 until n).map(_ => "_").mkString("|","|","|")
       bottom: String = |_|_|_|_|

       top + bottom
       res118: String = ._._._._.|_|_|_|_|

       s"$top\n$bottom"
       res120: String =
       ._._._._.
       |_|_|_|_|

     */

    def prettyPrint(solutions: List[Int]): String = {
      val topEdge = (1 to n).map( _ => "_" ).mkString( ".", ".", "." ) // ._._._._.
      val rows = solutions.map { queen =>
        val cellsBefore = (0 until queen).map( _ => "_" )
        val beforeString = if (cellsBefore.isEmpty) "|" else cellsBefore.mkString( "|", "|", "|" )
        val cellsAfter = ((queen + 1) until n).map( _ => "_" )
        val afterString = if (cellsAfter.isEmpty) "|" else cellsAfter.mkString( "|", "|", "|" )

        beforeString + "x" + afterString
      }
      s"$topEdge\n${rows.mkString( "\n" )}"
    }

    nQueensTailrec( 0, List(), List() ).map( prettyPrint )
  }


  // nQueens(4).foreach(println)

  val q8 = nQueens(8)
  val printableSolutions = q8.mkString("\n\n")

  println(printableSolutions)
  println(s"q8 chances are ${q8.length}" )



}
