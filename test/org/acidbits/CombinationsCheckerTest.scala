package org.acidbits
import org.junit._
import CombinationsChecker._
import scala.collection.mutable.ArrayBuffer
import Util._
// FIXME add expected = IllegalArgumentException
class CombinationsCheckerTest {
  
  @Test
  def testEmptyBoard() {
    val s = new CombinationsChecker
    val board = emptyBoard(0,0)
    s.countCombinations(board, King :: Nil)
  }
  
  @Test
  def testMorePiecesThanBoardSize() {
    val s = new CombinationsChecker
    val board = emptyBoard(3,3)
    s.countCombinations(board, King :: King :: King :: Nil)
  }
  
  @Test
  def testEmptyPieces()  {
    val s = new CombinationsChecker
    val board = emptyBoard(2,2)
    s.countCombinations(board, Nil)
  }
  
  
  
}