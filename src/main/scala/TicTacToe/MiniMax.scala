package TicTacToe

import scala.math._

object MiniMax {

	def isTie(board: Array[Array[String]]): Boolean = {

		board.foreach(row => if (row.mkString.contains(".")) return false)

		true
	}

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

	def minimax(board: Array[Array[String]], isAgent: Boolean): Int = {

		val result = evaluate(board)

		if (result == 10) return result
		if (result == -10) return result
		if (isTie(board)) return 0

		if (isAgent) {
			var bestScore = Int.MinValue

			for (x <- 0 to 2) {
				for (y <- 0 to 2) {

					if (board(x)(y).equals(".")) {

						board(x)(y) = "X"
						val score = minimax(board, false)
						board(x)(y) = "."

						if (score > bestScore) {
							bestScore = score
						}
					}
				}
			}
			bestScore
		} else {
			var bestScore = Int.MaxValue

			for (x <- 0 to 2) {
				for (y <- 0 to 2) {

					if (board(x)(y).equals(".")) {

						board(x)(y) = "O"
						val score = minimax(board, true)
						board(x)(y) = "."

						if (score < bestScore) {
							bestScore = score
						}
					}
				}
			}
			bestScore
		}
	}

	def findBestMove(board: Array[Array[String]]): (Int, Int) = {

		var bestScore = Int.MinValue
		var bestMove = (0, 0)

		for (x <- 0 to 2) {
			for (y <- 0 to 2) {
				if (board(x)(y).equals(".")) {
					board(x)(y) = "X"
					val score = minimax(board, false)
					board(x)(y) = "."

					if (score > bestScore) {
						bestScore = score
						bestMove = (x, y)
					}
				}
			}
		}

		bestMove
	}

	def main(args: Array[String]): Unit = {

		val board = Array(
			Array("X", "X", "O"),
			Array("O", "O", "X"),
			Array("X", "O", "X"),
		)

		val move = findBestMove(board)

		println(move)
	}
}
