import scala.io.StdIn._

object Main extends App {
  val n = readLine().toInt
  val m = readLine().toInt

  val mDouble = m.toDouble

  val xs = (0 until m)
    .map(_ => readLine()
      .split(' ')
      .map(_.toDouble)
      .toList)
    .toList
    .map { case x :: y :: Nil => (x, y) }


  val dx_y = xs.map { case (_, y) => y * y / mDouble }
    .sum

  val eyxs = xs.groupBy(_._1 - 1)
    .map { case (k, v) => {
      val len = v.length
      val sumY = v.map(_._2).sum

      (k, (sumY / mDouble, len / mDouble))
      }
    }.values.toList

  val e = eyxs.filter(_._2 != 0)
      .map { case (k, v) => k * k / v}
      .sum

  println(dx_y - e)

}