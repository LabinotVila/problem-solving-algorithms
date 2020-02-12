package TicTacToe

import scala.math._

object MiniMax {

	/** Checks whether the board is a tie */
	def isTie(board: Array[Array[String]]): Boolean = {
		board.foreach(row => if (row.mkString.contains(".")) return false)

		true
	}

	/** Heuristic approach */
	def evaluate(board: Array[Array[String]]): Int = {
		/** Check horizontally if someone has won */
		board.foreach(row => if (row.mkString.equals("XXX")) return 10
		else if (row.mkString.equals("OOO")) return -10)

		/** Check vertically if someone has won */
		board.transpose.foreach(row => if (row.mkString.equals("XXX")) return 10
		else if (row.mkString.equals("OOO")) return -10)

		/** Check slopes if someone has won */
		if (board(0)(0) == board(1)(1) && board(1)(1) == board(2)(2) && board(0)(0).equals("X")) return 10
		if (board(0)(0) == board(1)(1) && board(1)(1) == board(2)(2) && board(0)(0).equals("O")) return -10
		if (board(0)(2) == board(1)(1) && board(1)(1) == board(2)(0) && board(0)(2).equals("X")) return 10
		if (board(0)(2) == board(1)(1) && board(1)(1) == board(2)(0) && board(0)(2).equals("O")) return -10

		/** If no one has won, return 0 */
		0
	}

	/** Minimax score evaluator */
	def minimax(board: Array[Array[String]], isAgent: Boolean): Int = {

		val result = evaluate(board)

		if (result != 0) return result
		if (isTie(board)) return 0

		if (isAgent) {
			var bestScore = Int.MinValue

			for (x <- 0 until board.length) {
				for (y <- 0 until board.length) {
					if (board(x)(y).equals(".")) {
						board(x)(y) = "X"

						bestScore = max(minimax(board, false), bestScore)

						board(x)(y) = "."
					}
				}
			}
			bestScore
		} else {
			var bestScore = Int.MaxValue

			for (x <- 0 until board.length) {
				for (y <- 0 until board.length) {
					if (board(x)(y).equals(".")) {
						board(x)(y) = "O"

						bestScore = min(minimax(board, true), bestScore)

						board(x)(y) = "."
					}
				}
			}
			bestScore
		}
	}

	/** Find best move of the given board */
	def findBestMove(board: Array[Array[String]]): (Int, Int) = {

		var bestScore = Int.MinValue
		var bestMove = (0, 0)

		for (x <- 0 until board.length) {
			for (y <- 0 until board.length) {
				if (board(x)(y).equals(".")) {
					board(x)(y) = "X"
					val score = minimax(board, false)

					if (score > bestScore) {
						bestScore = score
						bestMove = (x, y)
					}

					board(x)(y) = "."
				}
			}
		}
		bestMove
	}

	/** Main function */
	def main(args: Array[String]): Unit = {

		val board = Array(
			Array("O", "X", "0"),
			Array("X", "X", "."),
			Array("O", "O", "."),
		)

		println(findBestMove(board))
	}

}