package retcalc

import org.scalactic.{Equality,TolerantNumerics,TypeCheckedTripleEquals}
import org.scalatest.{Matchers,WordSpec}
/**
  * @author Ayush Mittal
  */
class RetCalcSpec extends WordSpec with Matchers with TypeCheckedTripleEquals {

  implicit val doubleEquality: Equality[Double] =
    TolerantNumerics.tolerantDoubleEquality(0.0001)

  "RetCalc.futureCapital" should {
    "calculate the amount of savings I will have in n months" in {
      val actual = RetCalc.futureCapital(
        returns = FixedReturns(0.04), nbOfMonths = 25 * 12,
        netIncome = 3000, currentExpenses = 2000,
        initialCapital = 10000)
      val expected = 541267.1990
      actual should ===(expected)
    }
  }


  val params1 = RetCalcParams(
    nbOfMonthsInRetirement = 40 * 12,
    netIncome = 3000,
    currentExpenses = 2000,
    initialCapital = 10000)

  "RetCalc.nbOfMonthsSaving" should {
    "calculate how long I need to save before I can retire" in {
      val actual = RetCalc.nbOfMonthsSaving(returns = FixedReturns(0.04), params = params1)
      val expected = 23 * 12 + 1
      actual should ===(expected)
    }
  }

  val params2 = RetCalcParams(
    nbOfMonthsInRetirement = 40 * 12,
    netIncome = 1000,
    currentExpenses = 2000,
    initialCapital = 10000)

  "not loop forever if I enter bad parameters" in {
    val actual = RetCalc.nbOfMonthsSaving(returns = FixedReturns(0.04), params = params2)
    actual should === (Int.MaxValue)
  }

  val params = RetCalcParams(
    nbOfMonthsInRetirement = 40 * 12,
    netIncome = 3000,
    currentExpenses = 2000,
    initialCapital = 10000)

  "RetCalc.simulatePlan" should {
    "calculate the capital at retirement and the capital after death" in {
      val (capitalAtRetirement, capitalAfterDeath) =
        RetCalc.simulatePlan(
          returns = FixedReturns(0.04), params, nbOfMonthsSaving = 25*12)

      capitalAtRetirement should === (541267.1990)
      capitalAfterDeath should === (309867.5316)
    }

    "use different returns for capitalisation and drawdown" in {
      val nbOfMonthsSavings = 25 * 12
      val returns = VariableReturns(
        Vector.tabulate(nbOfMonthsSavings +
          params.nbOfMonthsInRetirement)(i =>
          if (i < nbOfMonthsSavings)
            VariableReturn(i.toString, 0.04 / 12)
          else
            VariableReturn(i.toString, 0.03 / 12)))
      val (capitalAtRetirement, capitalAfterDeath) =
        RetCalc.simulatePlan(returns, params, nbOfMonthsSavings)
      capitalAtRetirement should ===(541267.1990)
      capitalAfterDeath should ===(-57737.7227)
    }
  }
}
