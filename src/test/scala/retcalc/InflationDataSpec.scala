package retcalc

import org.scalatest.{Matchers, WordSpec}
/**
  * @author Ayush Mittal
  */
class InflationDataSpec extends WordSpec with Matchers {
  "InflationData.fromResource" should {
    "load CPI data from a tsv file" in {
      val data = InflationData.fromResource("cpi.tsv")
      data.size should ===(1413)
    }
  }
}
