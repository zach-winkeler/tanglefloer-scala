import algebras.Sign.{Negative, Positive}
import modules.{CATA, CDTD}
import tangles.ETangle
import tangles.ETangleType.Cup
import modules.Reduction._

object Main {
  def main(args: Array[String]): Unit = {
    val cup = new ETangle(Cup, IndexedSeq(Negative, Positive, Negative, Positive), 2)
    val cupDD = CDTD.from(cup)
    print(s"Before reduction (DD): ${cupDD.graph.nodes.size.toString}\n")
    cupDD.reduce()
    print(s"After reduction (DD): ${cupDD.graph.nodes.size.toString}\n")
    val cupAA = CATA.from(cup)
    print(s"Before reduction (AA): ${cupAA.graph.nodes.size.toString}\n")
    cupAA.reduce()
    print(s"After reduction (AA): ${cupAA.graph.nodes.size.toString}\n")
  }
}
