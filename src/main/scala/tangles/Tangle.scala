package tangles

import algebra.Sign.Sign
import tangles.ETangleType.{Cup, ETangleType, Cap}

object ETangleType extends Enumeration {
  type ETangleType = Value
  val Straight, Cup, Cap, Over, Under = Value
}

class ETangle(val kind: ETangleType, val signs: Vector[Sign], val pos: Int) {
  assert (pos < signs.length)
  if (kind == Cup || kind == Cap) {
    assert (signs(pos-1) == -signs(pos))
  }

  def height: Int = signs.length

  override def equals(other: Any): Boolean = other match {
    case other: ETangle => this.kind == other.kind && this.signs == other.signs && this.pos == other.pos
    case _ => false
  }

  override def hashCode(): Int = Seq(kind, signs, pos).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
}

class Tangle(val pieces: Vector[ETangle]) {
  def height: Int = pieces.map(_.height).max

  def +(other: Tangle): Tangle = new Tangle(this.pieces ++ other.pieces)

  override def equals(other: Any): Boolean = other match {
    case other: Tangle => this.pieces == other.pieces
    case _ => false
  }

  override def hashCode(): Int = Seq(pieces).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
}
