package actors.state

import org.specs2.mutable.Specification

class BoardSpec extends Specification {

  val someRandomBlockShape = 0

  "Embedding" should {
    "return the same board" in {
      val board = Board.empty(4, 4)
      val block = Block(
        Matrix {
          Vector(
            Vector(true, true, false),
            Vector(false, true, true)
          )
        }, 0, 0,
        someRandomBlockShape
      )

      // block is outside board
      board.embed(block) should be equalTo board
    }

    "embed block" in {
      val board = Board.empty(4, 4)
      val block = Block(
        Matrix {
          Vector(
            Vector(true, true, false),
            Vector(false, true, true)
          )
        }, 1, 3,
        someRandomBlockShape
      )

      board.embed(block).matrix should be equalTo Matrix {
        Vector(
          Vector(false, false, false, false),
          Vector(false, true, true, false),
          Vector(false, false, true, true),
          Vector(false, false, false, false)
        )
      }
    }


  }

}
