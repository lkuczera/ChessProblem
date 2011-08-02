package org.acidbits
import scala.collection.mutable.ArrayBuffer
import CombinationsChecker._
import Util._
import java.io._
object Tests {

      
  def main(args: Array[String]): Unit = {
    val s = new CombinationsChecker
    val board = emptyBoard(7, 8)
    var time = System.currentTimeMillis
    var combs = s.countCombinations(board, List(Queen, Queen))
    var combs3 = scala.collection.mutable.Set[Board]() 
    combs.foreach(brd => {
      	combs3 ++= s.countCombinations(brd, List(Rook, Rook))
    })
    println("combs3 size:"+combs3.size)
    println("done bishops")
    println("Took: "+ (System.currentTimeMillis-time)+"ms")
    var combs4 = scala.collection.mutable.Set[Board]()
    combs3.foreach(brd => {
      	combs4 ++= s.countCombinations(brd, List(Knight))
    })
    println("combs4 size:"+combs4.size)
    println("done Rook, King ")
    println("Took: "+ (System.currentTimeMillis-time)+"ms")
    var emptys = 0
    combs4.foreach(brd =>
      brd.foreach(_.foreach(piece => {
          if(piece == Empty) emptys += 1
      	}
        ))  
    )
    println("empty places: %d all: %d times: %s".format(emptys, combs4.size*56, (combs4.size*56.0)/emptys).toString)
    
    combs = null
    combs3 = null
    val combs5 = scala.collection.mutable.Set[Board]()
    combs4.foreach(brd => {
      	combs5 ++= s.countCombinations(brd, List(Knight))
    })
    var count = combs5.size
    
    println("done combs5 ")
    println("Took: "+ (System.currentTimeMillis-time)+"ms")
    combs4 = null
    count = 0
    System.gc
    Thread.sleep(100)
    time = System.currentTimeMillis
    val combs6 = scala.collection.mutable.Set[Board]()
    combs5.foreach(brd => {
      	combs6 ++= s.countCombinations(brd, List(King))
    })
    println("done combs6 ")
    println("no of unique combinations: "+count)
    println("Took: "+ (System.currentTimeMillis-time)+"ms")
    combs6.foreach(brd =>
      brd.foreach(_.foreach(piece => {
          if(piece == Empty) emptys += 1
      	}
        ))  
    )
    println("empty places: %d all: %d times: %s".format(emptys, combs4.size*56, (combs4.size*56.0)/emptys).toString)
    
//    time = System.currentTimeMillis
//    val combs2 = s.countCombinations(board, List(King, King, Queen, Queen))
//    println("Took: "+ (System.currentTimeMillis-time)+"ms")
//    println("no of unique combinations: "+combs2.size)
//    println
//    println("---------- DONE ----------")
//    println
//    combs.foreach(printBoard)
//      println("no of unique combinations: "+combs.size)
//    val time = System.currentTimeMillis
//    val solution = checkTrafiguraSet
//    solution.foreach(printBoard)
//    println("Possible combinations:" + solution.size)
//    println("Took: "+ (System.currentTimeMillis-time)+"ms")
  }

  def checkTrafiguraSet() = {
    val s = new CombinationsChecker
    val board = emptyBoard(7, 8)
    s.countCombinations(board, List(King, King, Queen, King, Queen)) //, Knight
  }

  def testUpdate() = {
    val s = new CombinationsChecker
    var board = emptyBoard(5, 5)
    s.updateVertical(board, (2, 2))
    println("update vertical")
    printBoard(board)
    board = emptyBoard(5, 5)
    s.updateHorizontal(board, (2, 2))
    println("update horizontal")
    printBoard(board)
    board = emptyBoard(5, 5)
    s.updateDiagonal(board, (2, 2))
    println("update diagonal")
    printBoard(board)

  }

}
