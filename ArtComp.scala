package NaiveArt

import java.time.LocalDateTime




object ArtComp {


  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////////////////                        Class Stuctures                     /////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


  case class Composition(
    size: Double,
    color: Double,
    lineAndShape: Double,
    texture: Double,
    value: Double,
    form: Double,
    space: Double,
    arrangement: Double,
    artist: String,
    context: Seq[(Double, Int)],
    date: LocalDateTime){
    def Attributes: Seq[Any] = Seq(size, color, lineAndShape, texture, value, form, space, arrangement, artist, context)}

  case class ArtPiece (
    name: String,
    composition: Composition){}

  case class ArtMarket(name: String, startDate: LocalDateTime, evalFunction: ArtPiece => Seq[IndexedSeq[(Int, Double)]], weights: ArtPiece => IndexedSeq[(Int, Seq[Double])]){
    def timeHorizon: IndexedSeq[Int] = for (d <- startDate.getYear to LocalDateTime.now().getYear by 1) yield d
  }

  case class ArtSignal(piece: ArtPiece, market: ArtMarket) {
    def compSignal: IndexedSeq[(Int, Seq[Double])] = combinedSignals(market.evalFunction(piece))
    def totalSignal: IndexedSeq[(Int, Double)] = overallSignal(compSignal, market.weights(piece))
  }


  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  /////////////////////////////                           Functions                        /////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  // Creates a constant time series to the present day
  def constantSeries(level: Double, startDate: LocalDateTime): IndexedSeq[(Int, Double)] = {
    val startYear: Int = startDate.getYear
    val currentYear: Int = LocalDateTime.now.getYear
    for (year <- startYear to currentYear by 1) yield (year, level)
  }

  // Creates a linearly interpolated time series from start date to end date
  def interpolateSeries(startVal: Double, startDate: LocalDateTime, endVal: Double, endDate: LocalDateTime): IndexedSeq[(Int, Double)] = {
    val startYear: Int = startDate.getYear
    val endYear: Int = endDate.getYear
    val years = for (year <- startYear to endYear by 1) yield year
    val increment = (endVal - startVal) / years.size
    years.map(Y => (Y, startVal + increment * (Y - startYear)))
  }

  // Function that matches "Artist Impact" to the Artist (Only for Pollock currently)
  def ArtistImpact(artPiece: ArtPiece): IndexedSeq[(Int, Double)] = {
    artPiece.composition.artist match {
      case "Jackson Pollock" => interpolateSeries(4, LocalDateTime.of(1948, 1, 1, 1, 1),
       10, LocalDateTime.of(1970,1,1,1,1))
      case "" => constantSeries(0, LocalDateTime.of(1940,1,1,1,1))
    }
  }

  // Given a target and a spread, normalizes to a Z between 1-10
  def evaluateValueNormal(pieceValue: Double, idealValue: Double, spread: Double): Double = {
    val Zval = Math.abs(pieceValue - idealValue) / spread
    val ZvalRounded = if (Zval > 3.0) 3.0 else Zval
    10 - ZvalRounded * (10/3)
  }

  //Given a sequence of Z's and weights, combines to form a signal
  def overallSignal(input: IndexedSeq[(Int, Seq[Double])], weights: IndexedSeq[(Int, Seq[Double])]): IndexedSeq[(Int, Double)] = {
    val years: IndexedSeq[Int] = input.map(I => I._1)
    val compoundSignals: IndexedSeq[Seq[Double]] = input.map(I => I._2)
    val signalWeights: IndexedSeq[Seq[Double]] = weights.map(W => W._2)
    val signal:IndexedSeq[Double] = for (i <- compoundSignals.indices) yield {
      (for (j <- compoundSignals.head.indices) yield {compoundSignals(i).apply(j) * signalWeights(i).apply(j)}).sum
    }
    years.zip(signal)
  }

  //Given a series of time series, combines to form one time series
  def combinedSignals(allSignals: Seq[IndexedSeq[(Int, Double)]]): IndexedSeq[(Int, Seq[Double])] = {
    val consolidatedSignals = for (i <- allSignals.head.indices) yield {
      for (j <- allSignals.indices) yield allSignals(j).map(V => V._2).apply(i)
    }
    val dates = allSignals.map(C => C.map(D => D._1)).last
    dates.zip(consolidatedSignals)
  }
}
