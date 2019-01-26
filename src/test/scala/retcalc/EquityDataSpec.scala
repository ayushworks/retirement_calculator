package retcalc

/**
  * @author Ayush Mittal
  */
import org.scalatest.{Matchers, WordSpec}

class EquityDataSpec extends WordSpec with Matchers {
  "EquityData.fromResource" should {
    "load market data from a tsv file" in {
      val data = EquityData.fromResource("sp500_2017.tsv")
      data.size should ===(1413)
    }
  }

  "EquityData.monthlyDividend" should {
    "return a monthly dividend" in {
      EquityData("2016.09", 2157.69, 45.03).monthlyDividend should ===(45.03 / 12)
    }
  }
}
