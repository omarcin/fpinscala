/**
 * Learning implicits with Scalaz Presentation https://vimeo.com/10482466
 */

trait Monoid[A]{
  val mzero : A
  val mappend : (A,A) => A
}

object Monoid {
  implicit object IntMonoid extends Monoid[Int] {
    override val mzero: Int = 0
    override val mappend: (Int, Int) => Int = _ + _
  }

  implicit object IntMultiMonoid extends Monoid[Int]  {
    override val mzero: Int = 1
    override val mappend: (Int, Int) => Int = _ * _
  }

  implicit object StringMonoid extends Monoid[String] {
    override val mzero: String = ""
    override val mappend: (String, String) => String = _ + _
  }
}

trait Foldable[F[_]] {
  def foldLeft[A,B](as : F[A])(seed : B) (f : (B,A) => B) : B
}

object Foldable {

  implicit object ListFoldable extends Foldable[List] {
    override def foldLeft[A, B](as: List[A])(seed: B)(f: (B, A) => B): B =
      as.foldLeft(seed)(f)
  }

  implicit object OptionFoldable extends Foldable[Option] {
    override def foldLeft[A, B](as: Option[A])(seed: B)(f: (B, A) => B): B =
      as.map(f(seed, _)).getOrElse(seed)
  }

}

trait FA[F[_], A] {
  val it : F[A]
  def reduceIt(implicit ma : Monoid[A], f : Foldable[F]) = {
    f.foldLeft(it)(ma.mzero)(ma.mappend)
  }
}

object FA {
  implicit def convert[F[_], A](f : F[A]) : FA[F, A] = {
    new FA[F, A] { val it = f}
  }
}


object Implicits01 {
  import FA._

  def test () = {
    println(List(1, 2, 3, 4).reduceIt(Monoid.IntMultiMonoid, implicitly[Foldable[List]]))
    println(List("Hello ", "world", "!").reduceIt)
    println((Some("TEST") : Option[String]).reduceIt)
    println((Some(100) :Option[Int]).reduceIt(Monoid.IntMonoid, implicitly[Foldable[Option]]))
    println((Option.empty[Int]).reduceIt(Monoid.IntMultiMonoid, implicitly[Foldable[Option]]))
  }
}
