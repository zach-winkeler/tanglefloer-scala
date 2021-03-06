package tangles

import algebras.Sign.{Negative, Positive, Sign}
import tangles.ETangleType.{Cap, Cup, ETangleType, Over, Straight, Under}

object ETangleType extends Enumeration {
  type ETangleType = Value
  val Straight, Cup, Cap, Over, Under = Value
}

class ETangle(val kind: ETangleType, val signs: IndexedSeq[Sign], val pos: Int) {
  assert (pos < signs.length)
  if (kind == Cup || kind == Cap) {
    assert (signs(pos) == -signs(pos+1))
  }

  def leftStrands: Set[DirectedStrand] = kind match {
    case Straight | Over => for ((sign, i) <- signs.zipWithIndex.toSet) yield DirectedStrand(i + 0.5f, i + 0.5f, sign)
    case Cup => for ((sign, i) <- signs.zipWithIndex.toSet if i < pos || i > pos+2) yield {
      i match {
        case _ if i < pos => DirectedStrand(i + 0.5f, i + 0.5f, sign)
        case _ if i > pos+2 => DirectedStrand(i + 0.5f, i + 1.5f, sign)
      }
    }
    case Cap => for ((sign, i) <- signs.zipWithIndex.toSet) yield {
      i match {
        case _ if i <= pos => DirectedStrand(i + 0.5f, i + 0.5f, sign)
        case _ if i >= pos+1 => DirectedStrand(i + 0.5f, i - 0.5f, sign)
      }
    }
    case Under => for ((sign, i) <- signs.zipWithIndex.toSet) yield {
      i match {
        case _ if i < pos || i > pos+2 => DirectedStrand(i + 0.5f, i + 0.5f, sign)
        case _ if i == pos => DirectedStrand(pos + 1.5f, pos + 0.5f, sign)
        case _ if i == pos+1 => DirectedStrand(pos + 0.5f, pos + 1.5f, sign)
      }
    }
  }

  def rightStrands: Set[DirectedStrand] = kind match {
    case Straight | Under => for ((sign, i) <- signs.zipWithIndex.toSet) yield DirectedStrand(i + 0.5f, i + 0.5f, sign)
    case Cup => for ((sign, i) <- signs.zipWithIndex.toSet) yield {
      i match {
        case _ if i <= pos => DirectedStrand(i + 0.5f, i + 0.5f, sign)
        case _ if i >= pos+1 => DirectedStrand(i - 0.5f, i + 0.5f, sign)
      }
    }
    case Cap => for ((sign, i) <- signs.zipWithIndex.toSet if i < pos || i > pos+2) yield {
      i match {
        case _ if i < pos => DirectedStrand(i + 0.5f, i + 0.5f, sign)
        case _ if i > pos+2 => DirectedStrand(i + 0.5f, i + 1.5f, sign)
      }
    }
    case Over => for ((sign, i) <- signs.zipWithIndex.toSet) yield {
      i match {
        case _ if i < pos || i > pos+2 => DirectedStrand(i + 0.5f, i + 0.5f, sign)
        case _ if i == pos => DirectedStrand(pos + 0.5f, pos + 1.5f, sign)
        case _ if i == pos+1 => DirectedStrand(pos + 1.5f, pos + 0.5f, sign)
      }
    }
  }

  def leftSigns: IndexedSeq[Set[Sign]] = kind match {
    case Cup => (signs.slice(0, pos) ++ signs.slice(pos+2, signs.length)).map(Set(_))
    case Under => (signs.slice(0, pos) ++: signs(pos+1) +: signs(pos) +: signs.slice(pos+2, signs.length)).map(Set(_))
    case _ => signs.map(Set(_))
  }

  def middleSigns: IndexedSeq[Set[Sign]] = kind match {
    case Cup => signs.slice(0, pos).map(Set(_)) ++
      IndexedSeq(Set(Positive, Negative)) ++
      signs.slice(pos+2, signs.length).map(Set(_))
    case Cap => signs.slice(0, pos).map(Set(_)) ++
      IndexedSeq(Set.empty[Sign]) ++
      signs.slice(pos+2, signs.length).map(Set(_))
    case _ => signs.map(Set(_))
  }

  def rightSigns: IndexedSeq[Set[Sign]] = kind match {
    case Cap => (signs.slice(0, pos) ++ signs.slice(pos+2, signs.length)).map(Set(_))
    case Over => (signs.slice(0, pos) ++: signs(pos+1) +: signs(pos) +: signs.slice(pos+2, signs.length)).map(Set(_))
    case _ => signs.map(Set(_))
  }

  def leftPoints: Set[Float] = (0 until leftSigns.length+1).map(_.toFloat).toSet

  def middlePoints: Set[Float] = (0 until middleSigns.length+1).map(_.toFloat).toSet

  def rightPoints: Set[Float] = (0 until rightSigns.length+1).map(_.toFloat).toSet

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
