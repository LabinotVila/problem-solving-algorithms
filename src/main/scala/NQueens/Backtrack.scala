package NQueens

import scala.util.Try

object Backtrack {

	/** Checks if the value to be placed is horizontally valid */
	def isHorizontallyGood(board: Array[Array[Int]], x: Int): Boolean = !board(x).contains(1)

	/** Checks if the value to be placed is vertically valid */
	def isVerticallyGood(board: Array[Array[Int]], y: Int): Boolean = {
		board.foreach(row => if (row(y).equals(1)) return false)

		true
	}

	/** Checks if the diagonals of the value to be placed are valid */
	def isSlopeGood(board: Array[Array[Int]], x: Int, y: Int): Boolean = {
		for (value <- x to board.length - 1) {
			val tryRightDiagonal = Try(board(value)(y + value - x)).getOrElse(null)
			val tryLeftDiagonal = Try(board(value)(y - value + x)).getOrElse(null)

			if (tryRightDiagonal == 1 || tryLeftDiagonal == 1) return false
		}

		for (value <- (board.length - 1 - x) to board.length - 1) {
			val tryRightDiagonal = Try(board(board.length - 1 - value)(y + value - (board.length - 1 - x))).getOrElse(null)
			val tryLeftDiagonal = Try(board(board.length - 1 - value)(y - value + (board.length - 1 - x))).getOrElse(null)

			if (tryRightDiagonal == 1 || tryLeftDiagonal == 1) return false
		}

		true
	}

	/** Composes horizontal, vertical and diagonals and checks if the value to be placed is valid */
	def isPositionedGood(board: Array[Array[Int]], x: Int, y: Int): Boolean =
		isHorizontallyGood(board, x) && isVerticallyGood(board, y) && isSlopeGood(board, x, y)

	/** Prints the solution of the board */
	def printSolution(board: Array[Array[Int]]): Unit = {
		println("Given board has been successfully solved with the following solution: ")

		board.foreach(row => println(row.mkString("  ")))
	}

	/** Checks if the board is solved */
	def isBoardSolved(board: Array[Array[Int]]): Boolean = {
		var queensFound = 0

		board.foreach(row => if(row.mkString(" ").contains("1")) queensFound = queensFound + 1)

		if (queensFound != board.length) return false

		true
	}

	/** Backtracks for solution */
	def tryBacktrack(board: Array[Array[Int]], row: Int): Boolean = {
		if (isBoardSolved(board)) {
			printSolution(board)

			return true
		}

		for (x <- 0 to board.length - 1) {
			if (isPositionedGood(board, row, x)) {
				board(row)(x) = 1

				if (tryBacktrack(board, row + 1)) return true

				board(row)(x) = 0
			}
		}

		false
	}

	/** Solve the board as NQueens */
	def solve(N: Int): Boolean = {
		var board = Array[Array[Int]]()

		for (_ <- 0 to N - 1) {
			var arr = Array[Int]()

			for (_ <- 0 to N - 1) arr = arr :+ 0

			board = board :+ arr
		}

		tryBacktrack(board, 0)
	}

	/** Main function */
	def main(args: Array[String]): Unit = {
		val N = 10

		println("Trying to solve the board with N = " + N + ".")
		val isSolvable = solve(N)

		if (!isSolvable) println("Given board has no solution!")
	}

}
