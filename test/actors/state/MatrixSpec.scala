package actors.state

import org.specs2.mutable.Specification

class MatrixSpec extends Specification {

  "Matrix bottom edge computation" should {

    "compute bottom edges Vector #1" in {
      val matrix: Matrix = Matrix {
        Vector(
          Vector(true, true),
          Vector(true, true)
        )
      }
      matrix.bottomEdgeDelta should be equalTo Vector(0, 0)
    }

    "compute bottom edges Vector #2" in {
      val matrix = Matrix {
        Vector(
          Vector(true, true),
          Vector(true, false),
          Vector(true, false)
        )
      }
      matrix.bottomEdgeDelta should be equalTo Vector(0, 2)
    }

    "compute bottom edges Vector #3" in {
      val matrix = Matrix {
        Vector(
          Vector(true, true, true),
          Vector(false, false, true)
        )
      }
      matrix.bottomEdgeDelta should be equalTo Vector(1, 1, 0)
    }

    "compute bottom edges Vector #4" in {
      val matrix = Matrix {
        Vector(
          Vector(true),
          Vector(true),
          Vector(true),
          Vector(true)
        )
      }
      matrix.bottomEdgeDelta should be equalTo Vector(0)
    }

  }

  "Merging two Matrix" should {
    "produce new Matrix" in {
      val matrixA = Matrix {
        Vector(
          Vector(false, false, false, false),
          Vector(false, false, false, false),
          Vector(false, false, false, false),
          Vector(false, false, false, false)
        )
      }
      val matrixB = Matrix {
        Vector(
          Vector(true, true),
          Vector(true, true)
        )
      }

      matrixA.eat(oth = matrixB) should be equalTo Matrix {
        Vector(
          Vector(true, true, false, false),
          Vector(true, true, false, false),
          Vector(false, false, false, false),
          Vector(false, false, false, false)
        )
      }

    }

    "produce new Matrix (offset)" in {
      val matrixA = Matrix {
        Vector(
          Vector(false, false, false, false),
          Vector(false, false, false, false),
          Vector(false, false, false, false),
          Vector(false, false, false, false)
        )
      }
      val matrixB = Matrix {
        Vector(
          Vector(true, true),
          Vector(true, true)
        )
      }

      matrixA.eat(matrixB, 1, 1) should be equalTo Matrix {
        Vector(
          Vector(false, false, false, false),
          Vector(false, true, true, false),
          Vector(false, true, true, false),
          Vector(false, false, false, false)
        )
      }

    }

    "produce new Matrix (overlapping)" in {
      val matrixA = Matrix {
        Vector(
          Vector(false, false, false, false),
          Vector(false, false, false, false),
          Vector(false, false, false, false),
          Vector(false, false, false, false)
        )
      }
      val matrixB = Matrix {
        Vector(
          Vector(true, true),
          Vector(true, true)
        )
      }

      matrixA.eat(matrixB, 1, -1) should be equalTo Matrix {
        Vector(
          Vector(false, true, true, false),
          Vector(false, false, false, false),
          Vector(false, false, false, false),
          Vector(false, false, false, false)
        )
      }

    }

  }

  "Matrix rotation" should {
    "rotate right (counter-clockwise)" in {
      val matrix = Matrix {
        Vector(
          Vector(true, true),
          Vector(true, false),
          Vector(true, false)
        )
      }
      matrix.rotateRight should be equalTo Matrix {
        Vector(
          Vector(true, true, true),
          Vector(false, false, true)
        )
      }
    }

    "rotate left (clockwise)" in {
      val matrix = Matrix {
        Vector(
          Vector(true, true),
          Vector(true, false),
          Vector(true, false)
        )
      }
      matrix.rotateLeft should be equalTo Matrix {
        Vector(
          Vector(true, false, false),
          Vector(true, true, true)
        )
      }
    }
  }

  "Matrix's apply method" should {
    "return correct values" in {
      val matrix = Matrix {
        Vector(
          Vector(true, true, false),
          Vector(false, true, false),
          Vector(true, true, false),
          Vector(true, true, true)
        )
      }

      matrix(0, 0) should beTrue
      matrix(1, 0) should beTrue
      matrix(2, 0) should beFalse

      matrix(0, 1) should beFalse
      matrix(1, 1) should beTrue
      matrix(2, 1) should beFalse

      matrix(0, 2) should beTrue
      matrix(1, 2) should beTrue
      matrix(2, 2) should beFalse

      matrix(0, 3) should beTrue
      matrix(1, 3) should beTrue
      matrix(2, 3) should beTrue
    }
  }

}
