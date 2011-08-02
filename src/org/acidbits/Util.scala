package org.acidbits
object Util {
  import CombinationsChecker._
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
    ArrayBuffer.fill(n, m)(Empty)
  }


}