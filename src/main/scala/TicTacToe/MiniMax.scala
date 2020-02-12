package TicTacToe

import scala.math._

object MiniMax {

	/** Prints the whole board */
	def printBoard(board: Array[Array[String]]): Unit = board.foreach(row => println(row.mkString(" ")))

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

	/** Agent vs Player */
	def loadGame(board: Array[Array[String]]): Unit = {
		println("Agent's turn to play!")
		val agentMove = findBestMove(board)
		board(agentMove._1)(agentMove._2) = "X"
		printBoard(board)
		if (evaluate(board) == 10) return println("Agent has won the game!")
		if (isTie(board)) return println("Game is a tie!")

		println("Your turn to play!")
		print("Place the coordinates of the array: ")
		val input = scala.io.StdIn.readLine().split(" ").map(value => value.toInt)
		board(input(0))(input(1)) = "O"
		printBoard(board)
		if (evaluate(board) == -10) return println("Player has won the game!")
		if (isTie(board)) return println("Game is a tie!")

		println("\n######################")
		loadGame(board)
	}

	/** Main function */
	def main(args: Array[String]): Unit = {

		val board = Array(
			Array(".", ".", "."),
			Array(".", ".", "."),
			Array(".", ".", "."),
		)

		loadGame(board)
	}

}