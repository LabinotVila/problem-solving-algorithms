import scala.util.Try

object NQueens {

	def isHorizontallyGood(queens: Array[Array[Int]], x: Int): Boolean = !queens(x).contains(1)

	def isVerticallyGood(queens: Array[Array[Int]], y: Int): Boolean = {
		queens.foreach(row => if (row(y).equals(1)) return false)

		true
	}

	def isSlopeGood(queens: Array[Array[Int]], x: Int, y: Int): Boolean = {
		for (value <- x to queens.length - 1) {
			val tryRightDiagonal = Try(queens(value)(y + value - x)).getOrElse(null)
			val tryLeftDiagonal = Try(queens(value)(y - value + x)).getOrElse(null)

			if (tryRightDiagonal == 1 || tryLeftDiagonal == 1) return false
		}

		for (value <- (queens.length - 1 - x) to queens.length - 1) {
			val tryRightDiagonal = Try(queens(queens.length - 1 - value)(y + value - (queens.length - 1 - x))).getOrElse(null)
			val tryLeftDiagonal = Try(queens(queens.length - 1 - value)(y - value + (queens.length - 1 - x))).getOrElse(null)

			if (tryRightDiagonal == 1 || tryLeftDiagonal == 1) return false
		}

		true
	}

	def isPositionedGood(queens: Array[Array[Int]], x: Int, y: Int): Boolean =
		isHorizontallyGood(queens, x) && isVerticallyGood(queens, y) && isSlopeGood(queens, x, y)

	def printSolution(queens: Array[Array[Int]]): Unit = {
		queens.foreach(row => {
			row.foreach(value => print(value + "  "))

			println()
		})
	}

	def isBoardSolved(queens: Array[Array[Int]]): Boolean = {
		var queensFound = 0
		val queensLength = queens.length - 1

		for (x <- 0 to queensLength)
			for (y <- 0 to queensLength)
				if (queens(x)(y).equals(1)) queensFound = queensFound + 1

		if (queensFound != queens.length) return false

		printSolution(queens)
		true
	}

	def Backtrack(queens: Array[Array[Int]], row: Int): Boolean = {
		if (isBoardSolved(queens)) return true

		for (x <- 0 to queens.length - 1) {
			if (isPositionedGood(queens, row, x)) {
				queens(row)(x) = 1

				if (Backtrack(queens, row + 1))
					return true

				queens(row)(x) = 0
			}
		}

		false
	}

	def main(args: Array[String]): Unit = {
		val N = 7
		var queens = Array[Array[Int]]()

		for (_ <- 0 to N - 1) {
			var arr = Array[Int]()

			for (_ <- 0 to N - 1) arr = arr :+ 0

			queens = queens :+ arr
		}

		Backtrack(queens, 0)
	}
}
