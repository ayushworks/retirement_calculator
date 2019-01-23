package retcalc

import scala.annotation.tailrec

/**
  * @author Ayush Mittal
  */

case class RetCalcParams(nbOfMonthsInRetirement: Int, netIncome: Int, currentExpenses: Int, initialCapital: Double)

object RetCalc {
  def nbOfMonthsSaving(returns: Returns, params: RetCalcParams) : Int = {
    @tailrec
    def loop(months: Int): Int = {
      val (_, capitalAfterDeath) = simulatePlan(returns = returns, params = params, nbOfMonthsSaving = months)
      if (capitalAfterDeath > 0.0)
        months
      else
        loop(months + 1)
    }

    import params._
    if(netIncome > currentExpenses)
      loop(0)
    else
      Int.MaxValue
  }


  def simulatePlan(returns: Returns, params: RetCalcParams, nbOfMonthsSaving: Int) :(Double, Double) = {
    import params._
    val capitalAtRetirement = futureCapital(returns, nbOfMonthsSaving, netIncome, currentExpenses, initialCapital)
    val capitalAtDeath = futureCapital(OffsetReturns(returns, nbOfMonthsSaving), nbOfMonthsInRetirement, 0, currentExpenses, capitalAtRetirement)
    (capitalAtRetirement, capitalAtDeath)
  }

  def futureCapital(returns: Returns, nbOfMonths: Int, netIncome: Int, currentExpenses: Int, initialCapital: Double):Double = {

    val monthlySaving = netIncome - currentExpenses

    def nextCapital(acc: Double, month:Int): Double = {
      acc*(1+ Returns.monthlyRate(returns, month)) + monthlySaving
    }

    (0 until nbOfMonths).foldLeft(initialCapital)(nextCapital)
  }

}
