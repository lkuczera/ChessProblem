package org.acidbits
import CombinationsChecker._
import Util._
/**
 * Run example set, board of size 7x8 with 2 Kings, 2 Queens, 2 Bishops, Knight and 3 Rooks 
 */
object Main {
  
  def main(args : Array[String]) : Unit = {
    val s = new CombinationsChecker
    val board = emptyBoard(7, 8)
    var combinations = scala.collection.mutable.Set[Board]()
    var time = System.currentTimeMillis
    val sol = s.countCombinations(board, List(King, King, Rook, Rook, Rook, Knight, Bishop, Bishop, Queen, Queen))
    val total = System.currentTimeMillis - time
    sol.foreach(printBoard)
	println("Took: "+total/1000.0+"s")
	println("solution size: " +sol.size)
	
  }
  /**
   * Saves solution object to solution.bin file.
   */
  def storeSolution(sol: Set[Board]) {
    import java.io._
	val fileWriter = new FileWriter(new File("solution.txt"))
    sol.foreach(board =>
    	fileWriter.write(boardToString(board))
	)
	fileWriter.close();
    val objectOut = new ObjectOutputStream(new FileOutputStream(new File("solution.bin")))
    objectOut.writeObject(sol)
    objectOut.close();
  }

}
