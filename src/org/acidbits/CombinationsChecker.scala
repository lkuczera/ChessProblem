package org.acidbits
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Set
import scala.annotation.tailrec

class CombinationsChecker {
  import CombinationsChecker._
  /**
   * Counts possible combinations of placing Sequence of Pieces on board.
   * If its not possible to place all pieces throws and Exception
   */
  def countCombinations(board: Board, pieces: Seq[Piece]): scala.collection.Set[Board] = {
    require(board.length > 0, "Board width must be > 0")
    require(board(0).length > 0, "Board length must be > 0")
    require(pieces.size > 0, "Pieces size must be > 0")
    val places = board.length * board(0).length
    // rough check if there is space for pieces
    require(pieces.size < places)
    // checks possible placements
    val threes = pieces.sortWith(_ > _).grouped(3)
    
    @tailrec
    def countRecur(boards: scala.collection.mutable.Set[Board], iter: Iterator[Seq[Piece]]): Set[Board] = {
      if(! iter.hasNext) {
        return boards
      }
      val pieces = iter.next
      countRecur(boards.flatMap(countPieceSetComb(_, pieces).keySet), iter)
    }
    val sol = countRecur(Set(board), threes)
    sol
  }

  /**
   * Cleans board. Board is mutable collection.
   */
  def cleanBoard(board: Board) = {
    //  println("cleaning up board")
    board.foreach(row =>
      for (i <- 0 to row.length - 1) row.update(i, Empty))
  }

  /**
   * Counts combinations for given piece set in given order.
   * Doesn't count permutations.
   */
  //@tailrec
  private def countPieceSetComb(board: Board, pieces: Seq[Piece]): scala.collection.mutable.Map[Board, Int] = {

    // final piece in set get all possible combinations (solutions)
    if (pieces.size == 1) {
      getPieceComb(board, pieces.head)
    } else {

      //advance the board trying to place a piece on each position (this does backing)
      // try to place next piece on the board in this pos 
      // if possible then place next one recursively if not advance one pos further

      val solutions = scala.collection.mutable.Map[Board, Int]()

      for (i <- 0 to (board.length - 1); j <- 0 to (board(0).length - 1)) {
        if (board(i)(j) == Empty) {
          val pos = (i, j)
          //      empties.map(pos => {
          if (canBePlacedHere(board, pieces.head, pos)) {
            val tmp = board.map(_.clone)
            putPiece(tmp, pieces.head, pos)
            solutions ++= countPieceSetComb(tmp, pieces.tail)
          } // advance if not possible to put piece here
          //        })
        }
      }
      solutions
    }
  }

  /**
   * Checks if not taking another piece by that move and if another piece or Taken on that position already
   */

  private def canBePlacedHere(board: Board, piece: Piece, pos: (Int, Int)): Boolean = {
    placingOK_?(board, piece, pos) && checkTaking(board, piece, pos)
  }

  /**
   * Returns all possible positions as this is last piece to be checked on board
   */
  def getPieceComb(board: Board, piece: Piece): scala.collection.mutable.Map[Board, Int] = {
    val solutions = scala.collection.mutable.Map[Board, Int]()
    //    toCheck.foreach(pos => {
    for (i <- 0 to board.length - 1; j <- 0 to board(0).length - 1) {
      val pos = (i, j)
      if (canBePlacedHere(board, piece, pos)) {
        val cloned = board.map(_.clone)
        putPiece(cloned, piece, pos)
        solutions += (cloned -> 1)
      }
    }
    //    })
    solutions
  }

  def findEmptyPlaces(board: Board) = {
    for (i <- 0 to board.length - 1; j <- 0 to board(0).length - 1; if (board(i)(j) == Empty)) yield (i, j)
  }

  def putPiece(board: Board, piece: Piece, pos: (Int, Int)) {
    putPieceWithoutBoardUpdate(board, piece, pos)
    updateBoard(board, piece, pos)
  }

  def putPieceWithoutBoardUpdate(board: Board, piece: Piece, pos: (Int, Int)) {
    // check x bounds
    require(pos._1 < board.length, "Position x out of board: "+pos+" board: "+board)
    // check y bounds (checks only first array)
    require(pos._2 < board(0).length, "Position y out of board: "+pos+" board: "+board)
    if (placingOK_?(board, piece, pos)) board(pos._1).update(pos._2, piece)
    else throw new PlacingException("Not possible, expected Empty, got: "+board(pos._1)(pos._2))
  }

  def placingOK_?(board: Board, piece: Piece, pos: (Int, Int)) = {
    val onBoard = board(pos._1)(pos._2)
    // its ok to put Taken on top of Taken
    if (onBoard == Empty || (onBoard == Taken && piece == Taken)) true
    else false
  }

  def updateBoard(board: Board, piece: Piece, pos: (Int, Int)) = {
    // no default case to catch errors
    piece match {
      case King => updateAround(board, pos)
      case Queen =>
        updateVertical(board, pos)
        updateHorizontal(board, pos)
        updateDiagonal(board, pos)
      case Rook =>
        updateVertical(board, pos)
        updateHorizontal(board, pos)
      case Bishop => updateDiagonal(board, pos)
      case Knight => updateKnight(board, pos)
    }
  }
  // helper functions
  def possible_?(piece: Piece) = (piece == Empty || piece == Taken)
  def possible_?(i: Int, j: Int)(implicit board: Board) = (board(i)(j) == Empty || board(i)(j) == Taken)
  /**
   * Does this piece beat another in this pos ?
   */
  def checkTaking(implicit board: Board, piece: Piece, pos: (Int, Int)): Boolean = {
    piece match {
      case King   => checkAround(board, pos)
      case Rook   => checkVertical(board, pos) && checkHorizontal(board, pos)
      case Bishop => checkDiagonal(board, pos)
      case Queen  => checkVertical(board, pos) && checkHorizontal(board, pos) && checkDiagonal(board, pos)
      case Knight => checkKnight(board, pos)
    }
  }
  def updateAround(board: Board, pos: (Int, Int)) = {
    val x = pos._1 - 1
    val y = pos._2 - 1
    for (i <- x to x + 2; j <- y to y + 2) {
      if (i == pos._1 && j == pos._2) {
        () // noop
      } else {
        try {
          if (!(board(i)(j) == Taken || board(i)(j) == Empty)) {
            throw new PlacingException("pos "+(i, j)+" already taken by "+board(i)(j))
          }
          putPieceWithoutBoardUpdate(board, Taken, (i, j))
        } catch {
          case e: IndexOutOfBoundsException => //ignore can't take out of board
        }
      }
    }
  }
  def checkAround(implicit board: Board, pos: (Int, Int)): Boolean = {
    val x = pos._1
    val y = pos._2
    for (i <- 0 to board.length - 1; j <- 0 to board(0).length - 1) {
      if ((i >= x - 1 && i <= x + 1) && (j >= y - 1 && j <= y + 1)) {
        if (!possible_?(i, j)) {
          return false;
        }
      }
    }
    true
  }

  def checkVertical(board: Board, pos: (Int, Int)): Boolean = updateVertical(board, pos, (board, pos) => ())
  def updateVertical(board: Board, pos: (Int, Int)): Boolean = updateVertical(board, pos, (board, pos) => putPieceWithoutBoardUpdate(board, Taken, pos))

  def updateVertical(board: Board, pos: (Int, Int), updateF: (Board, (Int, Int)) => Unit): Boolean = {
    val x = pos._1
    val y = pos._2
    val z = board(0).length - 1
    for (j <- 0 to (board(0).length - 1); if (j != y)) {

      val onBoard = board(x)(j)
      if (!possible_?(onBoard)) {
        return false;
      }
      updateF(board, (x, j))
    }
    true
  }

  def checkHorizontal(board: Board, pos: (Int, Int)): Boolean = updateHorizontal(board, pos, (board, pos) => ())
  def updateHorizontal(board: Board, pos: (Int, Int)): Boolean = updateHorizontal(board, pos, (board, pos) => putPieceWithoutBoardUpdate(board, Taken, pos))

  def updateHorizontal(board: Board, pos: (Int, Int), updateF: (Board, (Int, Int)) => Unit): Boolean = {
    val x = pos._1
    val y = pos._2
    for (i <- 0 to board.length - 1; if (i != x)) {
      val onBoard = board(i)(y)
      if (!possible_?(onBoard)) {
        return false;
      }
      updateF(board, (i, y))
    }
    true
  }

  def checkDiagonal(board: Board, pos: (Int, Int)): Boolean = updateDiagonal(board, pos, (board, pos) => ())
  def updateDiagonal(board: Board, pos: (Int, Int)): Boolean = updateDiagonal(board, pos, (board, pos) => putPieceWithoutBoardUpdate(board, Taken, pos))
  // FIXME - some issues with diagonal left to right updates
  def updateDiagonal(board: Board, pos: (Int, Int), updateF: (Board, (Int, Int)) => Unit): Boolean = {
    val dx = (pos._1 + 1) - (pos._2 + 1)
    val dy = (pos._2 + 1) - (pos._1 + 1)
    val x = if (dx > 0) dx else 0
    val y = if (dy > 0) dy else 0
    // diagonally from top left to bottom right
    for (i <- 0 to board.length - 1 - x; if ((x + i) != pos._1 && (y + i) != pos._2)) {
      try {
        //        println("x:%s, y:%s, dx:%s, dy:%s board.length-1: %s".format(x,y,dx,dy, board.length-1))
        val pos = (x + i, y + i)
        //        println("i: "+i)
        //        println("trying pos: "+pos)
        val onBoard = board(x + i)(y + i)

        if (!possible_?(onBoard)) {
          //          println("imposible !! found: "+onBoard+" at position:"+pos)
          return false;
        }
        //        println("ok updateting board")
        updateF(board, (x + i, y + i))
      } catch {
        case e: IndexOutOfBoundsException => // hit bottom
      }
    }
    // diagonally from position to top right
    for (i <- 1 to (board.length - 1)) {
      try {
        val onBoard = board(pos._1 + i)(pos._2 - i)
        if (!possible_?(onBoard)) {
          return false;
        }
        updateF(board, (pos._1 + i, pos._2 - i))
      } catch {
        case e: IndexOutOfBoundsException => // hit top
      }
    }

    // diagonally from position to bottom left
    for (i <- (1 to pos._1).reverse) {
      try {
        val onBoard = board(pos._1 - i)(pos._2 + i)
        if (!possible_?(onBoard)) {
          //          println("placing not possible found "+onBoard)
          return false;
        }
        updateF(board, (pos._1 - i, pos._2 + i))
      } catch {
        case e: IndexOutOfBoundsException => // hit bottom
      }
    }
    true
  }

  def checkKnight(board: Board, pos: (Int, Int)): Boolean = { //updateDiagonal(board, pos, (board, pos) => putPieceWithoutBoardUpdate(board, Taken, pos))
    val positions = List(
      (pos._1 - 2, pos._2 - 1),
      (pos._1 - 1, pos._2 - 2),
      (pos._1 + 2, pos._2 - 1),
      (pos._1 + 1, pos._2 - 2),
      (pos._1 - 2, pos._2 + 1),
      (pos._1 - 1, pos._2 + 2),
      (pos._1 + 2, pos._2 + 1),
      (pos._1 + 1, pos._2 + 2))
    positions.foreach(position => {
      try {
        if (!possible_?(board(position._1)(position._2))) {
          return false;
        }
      } catch {
        case e: IndexOutOfBoundsException => // out of board
      }
    })
    true
  }
  def updateKnight(board: Board, pos: (Int, Int)): Boolean = {
    val positions = List(
      (pos._1 - 2, pos._2 - 1),
      (pos._1 - 1, pos._2 - 2),
      (pos._1 + 2, pos._2 - 1),
      (pos._1 + 1, pos._2 - 2),
      (pos._1 - 2, pos._2 + 1),
      (pos._1 - 1, pos._2 + 2),
      (pos._1 + 2, pos._2 + 1),
      (pos._1 + 1, pos._2 + 2))
    positions.foreach(position => {
      try {
        board(position._1).update(position._2, Taken)
      } catch {
        case e: IndexOutOfBoundsException => // out of board
      }
    })
    true
  }
}

object CombinationsChecker {
  type Board = ArrayBuffer[ArrayBuffer[Piece]]
  type Piece = Int

  val Empty = 0
  val Taken = 1
  val King = 2
  val Bishop = 3
  val Rook = 4
  val Knight = 5
  val Queen = 6

  /**
   * Generate all possible empty combinations for board of size 8x8
   */
  lazy val emptiesMap = {
    val v = for (i <- 0 to 7; j <- 0 to 7) yield (i, j)
    val set = Set[(Int, Int)]() ++ v
    generateEmptiesSet(set)
    val empties = combinations
    println("invoking empties map")
    val hashCodes = empties.map(_.hashCode)
    // all hash codes must be unique
    require(hashCodes.size == hashCodes.toSet.size)
    hashCodes.zip(empties).toMap
  }

  private val combinations = Set[Set[(Int, Int)]]()

  @tailrec
  private def generateEmptiesSet(myList: Set[(Int, Int)]): Unit = {
    if (myList.size == 1) {
      for (i <- 0 to 7; j <- 0 to 7) {
        combinations += myList + ((i, j))
      }
      return ()
    }
    for (i <- 0 to 7; j <- 0 to 7) {
      combinations += myList + ((i, j))
    }
    generateEmptiesSet(myList.tail)
  }
}

class PlacingException(msg: String) extends RuntimeException(msg: String)