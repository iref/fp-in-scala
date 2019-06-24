package fpinscala.ch11

case class Id[A](value: A) {
   def flatMap[B](f: A => Id[A]): Id[B] = f(a)

   def map[B](f: A => B): Id[B] = Id(f(a))
}

object Id {
 
  def unit(a: A): Id[A] = Id(value)

  val idMonad = new Monad[Id] {
    def unit[A](a: A): Id[A] = Id.unit(a)

    def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] =
      fa.flatMap(f)
  }
}
