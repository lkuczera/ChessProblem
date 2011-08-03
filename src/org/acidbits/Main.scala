package org.acidbits
import CombinationsChecker._
import Util._

object Main {
  
  def main(args : Array[String]) : Unit = {
    
    val s = new CombinationsChecker
    val board = emptyBoard(7, 8)
    var combinations = scala.collection.mutable.Set[Board]()
    val time0 = System.currentTimeMillis
    var time = System.currentTimeMillis
    val sol = s.countCombinations(board, List(King, King, Rook, Rook, Rook, Knight, Bishop, Bishop, Queen, Queen))
	println("Took: "+(System.currentTimeMillis - time)/1000.0+"s")
	println("solution size: " +sol.size)
//	sol.foreach(printBoard)
	System.exit(0)
    s.countCombinations(board, List(Queen, Queen, Rook, Rook)).foreach(brd => {
      combinations ++= s.countCombinations(brd, List(Rook, Rook, Knight))
    }) 
    println("Done Queen, Queen, Rook, Rook, Rook, Knight")
    var combinations1 = scala.collection.mutable.Set[Board]()
    time = System.currentTimeMillis
    combinations.foreach(brd => {
      combinations1 ++= s.countCombinations(brd, List(Bishop, Bishop))
    })
    println("Done Queen, Queen, Rook, Rook, Rook, Knight, Bishop, Bishop")
    println("Took: "+(System.currentTimeMillis - time)+"ms")
    combinations = null
    System.gc
    val solution = scala.collection.mutable.Set[Board]()
    combinations1.foreach(brd => {
      solution ++= s.countCombinations(brd, List(King, King))
    })
    println("Took: "+(System.currentTimeMillis - time)+"ms")
    println("------ END ----- ")
    solution.foreach(printBoard)
    println("Total time: "+(System.currentTimeMillis - time)/1000 +"s")
    println("Number of unique combinations: "+solution.size)
  }
}
