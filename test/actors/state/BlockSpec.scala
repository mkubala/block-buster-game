package actors.state

import org.specs2.mutable.Specification

class BlockSpec extends Specification {

  "Block embedding detection" should {

    def assertEmbeddedAfter(block: Block, board: Board, moves: Int) = {
      val (_, results) = ((0 until 10).foldLeft((block, List.empty[Boolean]))) { (acc, _) =>
        val (b, r) = acc
        (b.oneRowDown, r ++ List(b.isEmbed(board)))
      }
      results.take(moves).exists(identity) should beFalse
      results.drop(moves).forall(identity) should beTrue
    }

    "notice block embedded" in {

      "when reach board's bottom" in {
        val board = Board.empty(5, 10)
        val block = Block(Matrix {
          Vector(
            Vector(true, false),
            Vector(true, true),
            Vector(true, false)
          )
        }, 1, 0)
        assertEmbeddedAfter(block, board, 10)
      }

      "when reach already embedded block" in {
        val board = Board.empty(5, 10).embed {
          Block(Matrix {
            Vector(
              Vector(true, false),
              Vector(true, true))
          }, 1, 9)
        }
        val block = Block(Matrix {
          Vector(
            Vector(true, false),
            Vector(true, true),
            Vector(true, false)
          )
        }, 1, 0)
        assertEmbeddedAfter(block, board, 7)
      }

      "when reach already embedded block #2" in {
        val board = Board.empty(5, 10).embed {
          Block(Matrix {
            Vector(
              Vector(false, true),
              Vector(true, true))
          }, 1, 9)
        }
        val block = Block(Matrix {
          Vector(
            Vector(true, false),
            Vector(true, true),
            Vector(true, false)
          )
        }, 1, 0)
        assertEmbeddedAfter(block, board, 8)
      }
    }
  }

}
