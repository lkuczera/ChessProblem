package org.acidbits
import scala.collection.mutable.{ArrayBuffer, Set}
import CombinationsChecker._

class Board extends ArrayBuffer[ArrayBuffer[Piece]] {
	//def getEmptyPlaces(): Set[(Int, Int)] = CombinationsChecker.emptiesMap(emptyPlacesRef)
	def getEmptyPlaces(hashCode: Int): Set[(Int, Int)] = CombinationsChecker.emptiesMap(hashCode)
}

