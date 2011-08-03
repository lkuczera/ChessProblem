package org.acidbits
import org.junit._
import org.junit.Assert._
import CombinationsChecker._
import scala.collection.mutable.ArrayBuffer
import Util._
// FIXME add expected = IllegalArgumentException
class CombinationsCheckerTest {
  
//  @Test
  def testEmptyBoard() {
    val s = new CombinationsChecker
    val board = emptyBoard(0,0)
    s.countCombinations(board, King :: Nil)
  }
//  @Test(expected =  Exception.class)
  def testMorePiecesThanBoardSize() {
    val s = new CombinationsChecker
    val board = emptyBoard(3,3)
    s.countCombinations(board, King :: King :: King :: Nil)
  }
  
//  @Test
  def testEmptyPieces()  {
    val s = new CombinationsChecker
    val board = emptyBoard(2,2)
    s.countCombinations(board, Nil)
  }
  
//  @Test
  def testQueenKing() {
    val s = new CombinationsChecker
    val board = emptyBoard(3,3)
    val sol = s.countCombinations(board, Queen ::  King :: King :: Nil)
    assertEquals(4, sol.size)
  }
  
//  @Test
  def testThreeQueens() {
    val s = new CombinationsChecker
    val board = emptyBoard(4,4)
    val sol = s.countCombinations(board, Queen ::  Queen :: Queen :: Nil)
    sol.foreach(printBoard)
    assertEquals(24, sol.size)
  }
  
  @Test
  def testQueen() {
    val s = new CombinationsChecker
    var board = emptyBoard(4,4)
    s.putPiece(board, Queen, (2,1))
    printBoard(board)
    assertEquals(4, board.flatMap(_.filter(_ == Empty)).size)
    
    board = emptyBoard(4,4)
    
    s.putPiece(board, Queen, (1,1))
    printBoard(board)
    assertEquals(4, board.flatMap(_.filter(_ == Empty)).size)
  }
  
}