package actors.state

case class Matrix(protected val m: Vector[Vector[Boolean]]) {
  val height: Int = m.length
  val width: Int = m.headOption.map(_.length).getOrElse(0)

  lazy val bottomEdgeDelta: Vector[Int] = rotateRight.m.map(_.prefixLength(b => !b))
  lazy val bottomEdgeOffsets: Vector[(Int, Int)] = bottomEdgeDelta.zipWithIndex

  def rotateLeft: Matrix = Matrix {
    (for {
      col <- (0 until width).reverse
      row <- m
    } yield row(col)).grouped(height)
  }

  def rotateRight: Matrix = Matrix {
    (for {
      col <- 0 until width
      row <- m.reverse
    } yield row(col)).grouped(height)
  }

  def apply(x: Int, y: Int): Boolean = m(y)(x)

  def eat(oth: Matrix, xOffset: Int = 0, yOffset: Int = 0): Matrix = {
    require(oth.width + xOffset <= width && oth.height + yOffset <= height, "consumed Matrix doesn't fit target Matrix")

    val coordinatesToFill: Vector[(Int, Int)] = for {
      (row, rowIndex) <- oth.m.zipWithIndex
      (_, colIndex) <- row.zipWithIndex
      if (oth(colIndex, rowIndex))
    } yield (colIndex + xOffset, rowIndex + yOffset)

    val mergedVectors: Vector[Vector[Boolean]] =
      (coordinatesToFill.foldLeft(m)) { (acc, pos) =>
        val (x, y) = pos
        if (acc.isDefinedAt(y) && acc(y).isDefinedAt(x))
          acc.updated(y, acc(y).updated(x, true))
        else acc
      }

    Matrix(mergedVectors)
  }

  def toMatrixString: String = m.map(_.map(b => if (b) '#' else '.').mkString).mkString("\n")

}

object Matrix {
  def apply(m: IndexedSeq[IndexedSeq[Boolean]]): Matrix =
    Matrix(m.map(_.toVector).toVector)

  def apply(m: Iterator[IndexedSeq[Boolean]]): Matrix = Matrix(m.toVector)

  def apply(width: Int, height: Int)(value: => Boolean): Matrix =
    Matrix(Array.fill(height)(Array.fill(width)(value).toVector).toVector)
}
