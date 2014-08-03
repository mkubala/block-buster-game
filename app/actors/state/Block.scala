package actors.state

import scala.util.Random

case class Block(val matrix: Matrix, x: Int = 0, y: Int = 0) {

  val boardX = x
  val boardY = y - matrix.height

  def isEmbed(board: Board): Boolean =
    matrix.bottomEdgeOffsets exists { offsets =>
      val (yOffset, xOffset) = offsets
      board(x + xOffset, y - yOffset)
    }

  def oneRowDown: Block = new Block(matrix, x, y + 1)
}

object Block {

  type Shape = Int

  def random: (Shape, Block) = {
    val shape = Random.nextInt(7)
    (shape, Block(matrixFrom(shape), 0, 0))
  }

  private val matrixFrom: Map[Shape, Matrix] = Map(
    0 -> Matrix {
      Vector(
        Vector(true, true, true, true)
      )
    },
    1 -> Matrix {
      Vector(
        Vector(true, false, false),
        Vector(true, true, true)
      )
    },
    2 -> Matrix {
      Vector(
        Vector(false, false, true),
        Vector(true, true, true)
      )
    },
    3 -> Matrix {
      Vector(
        Vector(true, true),
        Vector(true, true)
      )
    },
    4 -> Matrix {
      Vector(
        Vector(false, true, true),
        Vector(true, true, false)
      )
    },
    5 -> Matrix {
      Vector(
        Vector(false, true, false),
        Vector(true, true, true)
      )
    },
    6 -> Matrix {
      Vector(
        Vector(true, true, false),
        Vector(false, true, true)
      )
    }
  ).withDefault(_ => matrixFrom(Random.nextInt(7)))


}
