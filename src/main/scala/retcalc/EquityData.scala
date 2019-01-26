package retcalc

import scala.io.Source

/**
  * @author Ayush Mittal
  */
object EquityData {
  def fromResource(str: String) : Vector[EquityData] = {
    Source.fromResource(str).getLines().drop(1).map {
      line =>
        val fields = line.split("\t")
        EquityData(fields(0), fields(1).toDouble, fields(2).toDouble)
    }.toVector
  }


}

case class EquityData(monthId: String, value: Double, annualDividend: Double) {
  val monthlyDividend : Double = annualDividend/12;
}