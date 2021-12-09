package tangles

import algebras.AMinus
import algebras.Sign.Sign
import tangles.ETangleType.{Straight, Cup, Cap, Over, Under, ETangleType}

object ETangleType extends Enumeration {
  type ETangleType = Value
  val Straight, Cup, Cap, Over, Under = Value
}

class ETangle(val kind: ETangleType, val signs: IndexedSeq[Sign], val pos: Int) {
  assert (pos < signs.length)
  if (kind == Cup || kind == Cap) {
    assert (signs(pos-1) == -signs(pos))
  }

  def leftSigns: IndexedSeq[Sign] = kind match {
    case Cup => signs.slice(0, pos) ++ signs.slice(pos+2, signs.length)
    case Under => signs.slice(0, pos) ++: signs(pos+1) +: signs(pos) +: signs.slice(pos+2, signs.length)
    case _ => signs
  }

  def rightSigns: IndexedSeq[Sign] = kind match {
    case Cap => signs.slice(0, pos) ++ signs.slice(pos+2, signs.length)
    case Over => signs.slice(0, pos) ++: signs(pos+1) +: signs(pos) +: signs.slice(pos+2, signs.length)
    case _ => signs
  }

  def height: Int = signs.length

  override def equals(other: Any): Boolean = other match {
    case other: ETangle => this.kind == other.kind && this.signs == other.signs && this.pos == other.pos
    case _ => false
  }

  override def hashCode(): Int = Seq(kind, signs, pos).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
}


class Tangle(val pieces: IndexedSeq[ETangle]) {
  def height: Int = pieces.map(_.height).max

  def +(other: Tangle): Tangle = new Tangle(this.pieces ++ other.pieces)

  override def equals(other: Any): Boolean = other match {
    case other: Tangle => this.pieces == other.pieces
    case _ => false
  }

  override def hashCode(): Int = Seq(pieces).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
}
