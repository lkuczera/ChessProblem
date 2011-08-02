package org.acidbits
import CombinationsChecker._
object Util {
  def printBoard(board: Board) = {
	  println(boardToString(board))
  }

  def printPiece(piece: Piece) {
    print(pieceToString(piece))
  }
  /**
   * Could be overriden in pieces but its only for printing boards, "Taken" is more meaningful than "."
   */
  def pieceToString(piece: Piece) = {
    piece match {
      case King => "K"
      case Knight => "G"
      case Bishop => "B"
      case Queen => "Q"
      case Rook => "R"
      case Empty => " "
      case Taken => "."
    }
  }
  
  def boardToString(board: Board) = {
    val buf = new StringBuffer()
    
    def horizontalLine() = {
      for (i <- 0 to board.length * 2) buf.append("-")
      buf.append("\n")
    }
    
    horizontalLine
    for (y <- 0 to board(0).length - 1) {
      buf.append("|")
      for(x <- 0 to board.length - 1) {
    	  buf.append(pieceToString((board(x)(y))))
    	  buf.append("|")
      }
      buf.append("\n")
      horizontalLine
    }
    buf.toString
  }
  import scala.collection.mutable.ArrayBuffer
  def emptyBoard(n: Int, m: Int): Board = {
    val board = new Board
    for(i <- 0 to n-1) {
      board + new ArrayBuffer(m)
      for(j <- 0 to m-1) {
        board(i) + Empty
      }
    }
    println(board)
    board
  }


}