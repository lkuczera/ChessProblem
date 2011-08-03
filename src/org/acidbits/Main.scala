package org.acidbits
import CombinationsChecker._
import Util._
/**
 * Run Trafigura's set, Board of size 7x8 and 2 Kings, 2 Queens, 2 Bishops, Knight and 3 Rooks 
 */
object Main {
  
  def main(args : Array[String]) : Unit = {
    val s = new CombinationsChecker
    val board = emptyBoard(7, 8)
    var combinations = scala.collection.mutable.Set[Board]()
    val time0 = System.currentTimeMillis
    var time = System.currentTimeMillis
    val sol = s.countCombinations(board, List(King, King, Rook, Rook, Rook, Knight, Bishop, Bishop, Queen, Queen))
    sol.foreach(printBoard)
	println("Took: "+(System.currentTimeMillis - time)/1000.0+"s")
	println("solution size: " +sol.size)
  }
}
