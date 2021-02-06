import scala.io.StdIn._
import scala.collection.immutable.List

object Main extends App {
  val n = readLine().toInt

  def sqr(x: Double) = x * x
  def sqr(x: Long) = x * x

  def disp(x: List[Double]) = {
    val a = x.sum / x.length
    x.map(z => sqr(z - a)).sum
  }

  def ranks(x: List[Long]): List[Long] = {
    @scala.annotation.tailrec
    def r(a: List[(Long, Int)], res: Array[Long], prev: Long, cnt: Long): List[Long] = a match {
      case Nil => res.toList
      case (x, i) :: xs =>
        val newCnt = if (x == prev) cnt else cnt + 1
        res(i) = newCnt
        r(xs, res, x, newCnt)
    }

    x match {
      case Nil => Nil
      case _ =>
        val xz =x.zipWithIndex.sortBy(_._1)
        r(xz.tail, Array.ofDim[Long](x.length), xz.head._1, 0)
    }
  }

  def spirman(x: List[Long], y: List[Long]) = {
    x.length match {
      case 0 | 1 => 0d
      case len =>
        val s: Long = ranks(x).zip(ranks(y)).map { case (a, b) => sqr(a - b) }.sum

        1 - 6 * s / (len * (len - 1d) * (len + 1d))
    }
  }

  val (xs, ys) = (0 until n)
    .map(_ => readLine()
      .split(' ')
      .map(_.toLong)
      .toList)
    .toList
    .map {case x :: y :: Nil => (x, y)}
    .unzip

  println(spirman(xs, ys))


}