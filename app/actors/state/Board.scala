package actors.state

object Board {
  def empty(width: Int, height: Int): Board =
    Board(Matrix(width, height)(false))
}

case class Board(matrix: Matrix) {

  val width = matrix.width

  val height = matrix.height

  def apply(col: Int, row: Int): Boolean =
    if (row >= height) true
    else if (row < 0 || col < 0 || col >= width) false
    else matrix(col, row)

  def embed(block: Block): Board =
    copy(matrix = matrix.eat(block.matrix, block.boardX, block.boardY))

}