package NaiveArt

import java.time.LocalDateTime


object Main {

  import ArtComp._
  import breeze.linalg._
  import breeze.plot._

  // Example Art Piece
  def No5 = ArtPiece("No. 5, 1948",
    composition = Composition(
      size = 9.5,
      color = 6.5,
      lineAndShape = .5,
      texture = 7.5,
      value = 2.5,
      form = 0.2,
      space = 0.8,
      arrangement = 4.5,
      artist = "Jackson Pollock",
      context = Seq((10.0, 2019)),
      date = LocalDateTime.of(1948, 1, 1, 1, 1)
    )
  )

  //Instantiating Z parameters for all attributes
  def ArtQuant(artPiece: ArtPiece): Seq[IndexedSeq[(Int, Double)]] = {
    val DateOfArtNormalization = LocalDateTime.of(1985, 1, 1, 1, 1)
    val After = LocalDateTime.of(1986, 1, 1, 1, 1)

    def sizeEval:  IndexedSeq[(Int, Double)] = constantSeries(artPiece.composition.size, artPiece.composition.date)
    def sizeIdeal: IndexedSeq[(Int, Double)] = interpolateSeries(7, artPiece.composition.date, 9, DateOfArtNormalization) ++ constantSeries(9, After)
    def sizeSpread: IndexedSeq[(Int, Double)] = constantSeries(2, artPiece.composition.date)
    def sizeZ: IndexedSeq[(Int, Double)] = combinedSignals(Seq(sizeEval, sizeIdeal, sizeSpread)).map(A => (A._1, evaluateValueNormal(A._2(0), A._2(1), A._2(2))))

    def colorEval: IndexedSeq[(Int, Double)] = constantSeries(artPiece.composition.color, artPiece.composition.date)
    def colorIdeal: IndexedSeq[(Int, Double)] = interpolateSeries(8, artPiece.composition.date, 7.0, DateOfArtNormalization) ++ constantSeries(7, After)
    def colorSpread: IndexedSeq[(Int, Double)] = constantSeries(2.5, artPiece.composition.date)
    def colorZ: IndexedSeq[(Int, Double)] = combinedSignals(Seq(colorEval, colorIdeal, colorSpread)).map(A => (A._1, evaluateValueNormal(A._2(0), A._2(1), A._2(2))))

    def lineAndShapeEval:  IndexedSeq[(Int, Double)] = constantSeries(artPiece.composition.lineAndShape, artPiece.composition.date)
    def lineAndShapeIdeal: IndexedSeq[(Int, Double)] = interpolateSeries(1, artPiece.composition.date, .5, DateOfArtNormalization) ++ constantSeries(.5, After)
    def lineAndShapeSpread: IndexedSeq[(Int, Double)] = constantSeries(1.5, artPiece.composition.date)
    def lineAndShapeZ: IndexedSeq[(Int, Double)] = combinedSignals(Seq(lineAndShapeEval, lineAndShapeIdeal, lineAndShapeSpread)).map(A => (A._1, evaluateValueNormal(A._2(0), A._2(1), A._2(2))))

    def textureEval:  IndexedSeq[(Int, Double)] = constantSeries(artPiece.composition.texture, artPiece.composition.date)
    def textureIdeal: IndexedSeq[(Int, Double)] = interpolateSeries(7, artPiece.composition.date, 7.5, DateOfArtNormalization) ++ constantSeries(7.5, After)
    def textureSpread: IndexedSeq[(Int, Double)] = constantSeries(1, artPiece.composition.date)
    def textureZ: IndexedSeq[(Int, Double)] = combinedSignals(Seq(textureEval, textureIdeal, textureSpread)).map(A => (A._1, evaluateValueNormal(A._2(0), A._2(1), A._2(2))))

    def valueEval:  IndexedSeq[(Int, Double)] = constantSeries(artPiece.composition.value, artPiece.composition.date)
    def valueIdeal: IndexedSeq[(Int, Double)] = interpolateSeries(2, artPiece.composition.date, 2.5, DateOfArtNormalization) ++ constantSeries(2.5, After)
    def valueSpread: IndexedSeq[(Int, Double)] = constantSeries(2, artPiece.composition.date)
    def valueZ: IndexedSeq[(Int, Double)] = combinedSignals(Seq(valueEval, valueIdeal, valueSpread)).map(A => (A._1, evaluateValueNormal(A._2(0), A._2(1), A._2(2))))

    def formEval:  IndexedSeq[(Int, Double)] = constantSeries(artPiece.composition.form, artPiece.composition.date)
    def formIdeal: IndexedSeq[(Int, Double)] = interpolateSeries(1, artPiece.composition.date, .2, DateOfArtNormalization) ++ constantSeries(.2, After)
    def formSpread: IndexedSeq[(Int, Double)] = constantSeries(2, artPiece.composition.date)
    def formZ: IndexedSeq[(Int, Double)] = combinedSignals(Seq(formEval, formIdeal, formSpread)).map(A => (A._1, evaluateValueNormal(A._2(0), A._2(1), A._2(2))))

    def spaceEval:  IndexedSeq[(Int, Double)] = constantSeries(artPiece.composition.space, artPiece.composition.date)
    def spaceIdeal: IndexedSeq[(Int, Double)] = interpolateSeries(2, artPiece.composition.date, 0.9, DateOfArtNormalization) ++ constantSeries(0.9, After)
    def spaceSpread: IndexedSeq[(Int, Double)] = constantSeries(1.5, artPiece.composition.date)
    def spaceZ: IndexedSeq[(Int, Double)] = combinedSignals(Seq(spaceEval, spaceIdeal, spaceSpread)).map(A => (A._1, evaluateValueNormal(A._2(0), A._2(1), A._2(2))))

    def arrangementEval:  IndexedSeq[(Int, Double)] = constantSeries(artPiece.composition.arrangement, artPiece.composition.date)
    def arrangementIdeal: IndexedSeq[(Int, Double)] = interpolateSeries(5.5, artPiece.composition.date, 4.5, DateOfArtNormalization) ++ constantSeries(4.5, After)
    def arrangementSpread: IndexedSeq[(Int, Double)] = constantSeries(1.5, artPiece.composition.date)
    def arrangementZ: IndexedSeq[(Int, Double)] = combinedSignals(Seq(arrangementEval, arrangementIdeal, arrangementSpread)).map(A => (A._1, evaluateValueNormal(A._2(0), A._2(1), A._2(2))))

    def artistZ: IndexedSeq[(Int, Double)] = ArtistImpact(artPiece) ++ constantSeries(10, LocalDateTime.of(1971, 1, 1, 1, 1))
    def contextZ: IndexedSeq[(Int, Double)] = interpolateSeries(5, artPiece.composition.date, artPiece.composition.context.head._1, LocalDateTime.now)
    Seq(sizeZ, colorZ, lineAndShapeZ, textureZ, valueZ, formZ, spaceZ, arrangementZ, artistZ, contextZ)
  }

  // Instantiating Weights for Abstract Expressionism
  def ArtWeights(artPiece: ArtPiece): IndexedSeq[(Int, Seq[Double])] = {
    def size = constantSeries(0.090, artPiece.composition.date)
    def color = constantSeries(0.125, artPiece.composition.date)
    def lineAndShape = constantSeries(0.143, artPiece.composition.date)
    def texture = constantSeries(0.053, artPiece.composition.date)
    def value = constantSeries(0.035, artPiece.composition.date)
    def form = constantSeries(0.072, artPiece.composition.date)
    def space = constantSeries(0.054, artPiece.composition.date)
    def arrangement = constantSeries(0.143, artPiece.composition.date)
    def artist = constantSeries(0.179, artPiece.composition.date)
    def context = constantSeries(0.107, artPiece.composition.date)
    def dates = size.map(D => D._1)
    def weightVals = for (i <- dates.indices) yield Seq(size.map(V => V._2).apply(i),
      color.map(V => V._2).apply(i), lineAndShape.map(V => V._2).apply(i), texture.map(V => V._2).apply(i),
      value.map(V => V._2).apply(i), form.map(V => V._2).apply(i), space.map(V => V._2).apply(i),
      arrangement.map(V => V._2).apply(i), artist.map(V => V._2).apply(i), context.map(V => V._2).apply(i))
    dates.zip(weightVals)
  }

  // Art Market for example
  def AbstractExpressionism = ArtMarket("Abstract Expressionism",
    startDate = LocalDateTime.of(1940, 1, 1, 1, 1),
    evalFunction = ArtQuant,
    weights = ArtWeights
  )

  // Plotting
  def main(args: Array[String]) = {
    val Signal: IndexedSeq[(Int, Double)] = ArtSignal(No5, AbstractExpressionism).totalSignal
    val timeSeries = DenseVector(Signal.map(T => T._1.toDouble).toArray)
    val signalSeries = DenseVector(Signal.map(V => V._2).toArray)
    val fig = Figure()
    val plt = fig.subplot(0)
    plt += plot(timeSeries, signalSeries)
    plt.title = "Jackson Pollock's No.5 Signal"
    plt.xlabel = "Year"
    plt.ylabel = "Signal (0-10)"
    plt.ylim(0, 10)
    fig.saveas("SignalGraph.png")
  }

}