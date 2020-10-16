package Graph

import com.google.gson.{Gson, JsonParser}
import scala.io.Source

/**
 *
 * We want to find all possible paths that go, in our case, from 2 to 3 in the directed graph
 *
 * *     -> indicates a starting point
 * >     -> indicates unidirectional link
 * <>    -> indicates bidirectional link
 *
 * The problem is located in 'data.json' and a preview of it looks like below:
 *
 * 2 <--------------> 0 *-------------> 3
 * *                   |            />
 * |                   |           |
 * |                   |     -----/
 * |                   |    |
 * \          /--> 1 </    |
 * \--------|     *-----/
 */

object Backtrack {

	/** We use gson to deserialize a json object into a class and json to enable us it's operations */
	val gson = new Gson()
	val json = new JsonParser()

	/** Visited holds the list of visited nodes, while solution gives us the final results */
	var visited: List[Node] = List()
	var solution: List[List[Node]] = List()

	/** Main function */
	def main(args: Array[String]): Unit = {
		val model: Tree = gson.fromJson(json.parse(Source.fromFile("src/main/scala/Path/data.json").mkString), classOf[Tree])

		val startNode: Option[Node] = model.nodes.find(node => node.id == model.from)
		val endNode: Option[Node] = model.nodes.find(node => node.id == model.to)

		if (startNode.isEmpty || endNode.isEmpty) {
			println("Can not find starting node or ending node in the tree!")

			return
		}

		println(s"Finding all paths from ${startNode.get.id} to ${endNode.get.id}.")

		visited = visited :+ startNode.get

		backtrack(model.nodes.toList, startNode.get, endNode.get)

		prettyPrint(solution)
	}

	/** Backtrack function */
	def backtrack(nodes: List[Node], currentNode: Node, lastNode: Node): Boolean = {
		if (currentNode.id == lastNode.id) {
			solution = solution :+ visited

			return false
		}

		currentNode.links.foreach(id => {
			val thisNode: Node = nodes.find(x => x.id == id).get

			if (!visited.contains(thisNode)) {
				visited = visited :+ thisNode

				if (backtrack(nodes, thisNode, lastNode)) return true

				visited = visited.filter(x => !x.equals(thisNode))
			}
		})

		false
	}

	/** Pretty print the solution */
	def prettyPrint(solution: List[List[Node]]): Unit = {
		var solutionNumber: Int = 0

		solution.foreach(solution => {
			solutionNumber = solutionNumber + 1

			println("Solution number " + solutionNumber + ": [" + solution.map(node => node.id).mkString(", ") + "]")
		})
	}

}
