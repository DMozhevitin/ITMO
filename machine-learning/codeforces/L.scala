import scala.io.StdIn._

object Main extends App {
  val n = readLine().toInt

  def sqr(x: Double) = x * x

  def disp(x: List[Double]) = {
    val a = x.sum / x.length
    x.map(z => sqr(z - a)).sum
  }

  def cov(x: List[Double], y: List[Double]) = {
    val avgX = x.sum / x.length
    val avgY = y.sum / y.length

    x.zip(y).map { case (a, b) => (a - avgX) * (b - avgY)}.sum
  }

  def pirson(x: List[Double], y: List[Double]) = {
    val dispX = disp(x)
    val dispY = disp(y)

    if (dispX == 0 || dispY == 0) 0 else cov(x, y) / Math.sqrt(dispX * dispY)
  }

  val (xs, ys) = (0 until n)
    .map(_ => readLine()
      .split(' ')
      .map(_.toDouble)
      .toList)
    .toList
    .map {case x :: y :: Nil => (x, y)}
    .unzip

  println(pirson(xs, ys))


}