package tangles

import algebra.Sign.Sign
import algebra.{Z2Polynomial, Z2PolynomialRing}

class StrandDiagram(val module: StrandDiagramSpan,
                    val blackStrands: Map[Float, Float],
                    val orangeStrands: Map[Float, Float],
                    val orangeSigns: Vector[Sign],
                    val orangeVars: Vector[Z2Polynomial]) {
  def toElement: StrandDiagramSpanElement = new StrandDiagramSpanElement(module, Map(this -> module.ring.one))

  def canEqual(other: Any): Boolean = other.isInstanceOf[StrandDiagram]

  override def equals(other: Any): Boolean = other match {
    case that: StrandDiagram =>
      (that canEqual this) &&
        module == that.module &&
        blackStrands == that.blackStrands &&
        orangeStrands == that.orangeStrands &&
        orangeSigns == that.orangeSigns &&
        orangeVars == that.orangeVars
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(blackStrands, orangeStrands, orangeSigns, orangeVars)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

class StrandDiagramSpan(val module: StrandDiagramSpan) {
  val zero: StrandDiagramSpanElement =
    new StrandDiagramSpanElement(this, Map.empty[StrandDiagram, Z2Polynomial])
}

class StrandDiagramSpanElement(val module: StrandDiagramSpan, val terms: Map[StrandDiagram, Z2Polynomial]) {

  def +(other: StrandDiagramSpanElement): StrandDiagramSpanElement = {
    assert (this.module == other.module)
    new StrandDiagramSpanElement(this.module, this.terms |+| other.terms)
  }

  def *(other: StrandDiagramSpanElement): StrandDiagramSpanElement = {
    assert (this.module == other.module)
    var result = this.module.zero
    for ((g1, c1) <- this.terms; (g2, c2) <- other.terms) {
      result += (c1 * c2) *: (g1 * g2)
    }
    result
  }

  def *:(scalar: Z2Polynomial): StrandDiagramSpanElement =
    new StrandDiagramSpanElement(module, terms.mapValues(scalar * _).toMap)

  override def equals(other: Any): Boolean = other match {
    case other: StrandDiagramSpanElement => this.module == other.module && this.terms == other.terms
    case _ => false
  }

  override def hashCode: Int = Seq(terms).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)

  override def toString: String = terms.mkString(" + ") match { case "" => "0" case s => s }
}
