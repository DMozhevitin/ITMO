import scala.io.StdIn._

object Main extends App {
  val input = readLine.split(' ').map(_.toInt)
  val n = input(0)
  val m = input(1)

  val dataset: List[(List[Double], Double)] = (0 until n)
    .map(_ => readLine()
      .split(' ')
      .map(_.toDouble))
    .map(x => (x.slice(0, m).toList, x(m)))
    .toList

  val targetObj = readLine().split(' ').map(_.toDouble).toList

  def namedDist(name: String, x: List[Double])(y: List[Double]): Double = {
    name match {
      case "manhattan" => x.zip(y).map { case (a, b) => Math.abs(a - b) }.sum
      case "euclidean" => Math.sqrt(x.zip(y).map { case (a, b) => (a - b) * (a - b) }.sum)
      case "chebyshev" => x.zip(y).map { case (a, b) => Math.abs(a - b) }.max
    }
  }

  def finite(q: Double) = Math.abs(q) < 1

  def kernel(name: String, u: Double): Double = {
    name match {
      case "uniform" => if (finite(u)) 0.5 else 0
      case "triangular" => if (finite(u)) 1 - u else 0
      case "epanechnikov" => if (finite(u)) 0.75 * (1 - (u * u)) else 0
      case "quartic" => if (finite(u)) (15d / 16d) * Math.pow(1 - u * u, 2) else 0
      case "triweight" => if (finite(u)) (35d / 32d) * Math.pow(1 - u * u, 3) else 0
      case "tricube" => if (finite(u)) (70d / 81d) * Math.pow(1 - Math.pow(u, 3), 3) else 0
      case "gaussian" => Math.exp(-0.5 * u * u) / Math.sqrt(2 * Math.PI)
      case "cosine" => if (finite(u)) (Math.PI / 4) * Math.cos(Math.PI * u / 2) else 0
      case "logistic" => 1d / (Math.exp(u) + 2 + Math.exp(-u))
      case "sigmoid" => (2d / Math.PI) / (Math.exp(u) + Math.exp(-u))
    }
  }

  val distName = readLine()
  val kernelName = readLine()
  val windowType = readLine()
  val h = readLine().toInt

  def distToTarget(q: List[Double]) = namedDist(distName, targetObj)(q)

  val objects = dataset.map {
    case (xs, y) => Obj(xs, y, distToTarget(xs))
  }.sortBy(_.dist)

  val h1 = windowType match {
    case "fixed" => h
    case "variable" => objects(h).dist
  }

  print(h1 match {
    case 0 =>
      val ne = (if (objects.head.x.equals(targetObj))
        objects.filter(_.x.equals(targetObj))
      else
        objects).map(_.y)
      ne.sum / ne.length
    case _ =>
      val b = objects.map(z => kernel(kernelName, z.dist / h1)).sum

      b match {
        case 0 => objects.map(_.y).sum / n
        case denom => objects.map(z => z.y * kernel(kernelName, z.dist / h1)).sum / denom
      }
  })
}

case class Obj(x: List[Double], y: Double, dist: Double) {
  override def toString: String = s"(${x.toString()}, ${y}, ${dist})"
}