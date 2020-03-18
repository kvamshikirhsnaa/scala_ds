package part2numbers

import scala.annotation.tailrec

object Test5 extends App {


  def firstNonRepeativeChar(s: String): Char = {
    @tailrec
    def nonRepeativeCharTailrec(ind: Int, m: Map[Char, Int]): Char = {
      if (ind >= s.length) '_'
      else if (m(s(ind)) == 1) s(ind)
      else nonRepeativeCharTailrec(ind + 1, m)
    }
    val aMap = Map[Char, Int]()
    val len = Range( 0, s.length )
    val tempMap = len.foldLeft( aMap ) {
      (tempx, curr) => {
        if (!tempx.contains(s(curr))) tempx + (s(curr) -> 1)
        else tempx + (s(curr) -> (tempx(s(curr)) + 1))
      }
    }
    nonRepeativeCharTailrec(0, tempMap)
  }


  val str = "ababcadbabea"
  val str2 = "nfewkjrferhfjkdshferuwhgrsvfbskjfesygelawpqeiufenmzcbzfewwdamcaawfanvdefhefwe"
  val str3 = "aaaabbbbbccccc"

  println( firstNonRepeativeChar( str ) )  // c
  println(firstNonRepeativeChar(str2))   // y
  println(firstNonRepeativeChar(str3))  // _


  def firstDuplicate(lst: List[Int]): Int = {
    @tailrec
    def firstDupTailrec(ind: Int, acc: List[Int]): Int = {
      if (ind >= lst.length) throw new RuntimeException("No Duplicate Element")
      else if (acc.contains(lst(ind))) lst(ind)
      else firstDupTailrec(ind + 1, lst(ind) +: acc)
    }
    firstDupTailrec(0, List.empty)
  }


  val lst = List(2,1,3,5,3,2)
  val lst2 = List(2,1,3,1,5,3,2,5)
  val lst3 = List(2,3,1,4,6,5)
  println(firstDuplicate(lst))
  println(firstDuplicate(lst2))
  println(firstDuplicate(lst3))



}
