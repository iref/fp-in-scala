package fpinscala.ch04

object Chapter4 {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap { m => 
      val xsVariances = xs.map(x => math.pow(x - m, 2))
      mean(xsVariances)
    }

  def mean_2(xs: Seq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("xs is empty")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try
      Right(x / y)
    catch { 
      case e: Exception => Left(e)
    }

  def Try[A](a: => A): Either[Exception, A] =
    try {
      Right(a)
    } catch {
      case e: Exception => Left(e)
    }

}
