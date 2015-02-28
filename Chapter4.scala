sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](default: => Option[B]): Option[B] = this match {
    case None => default
    case Some(_) => this
  }

  def orElse2[B >: A](default: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse default

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def flatMap2[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None


  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(a) => if (f(a)) Some(a) else None
  }

  def filter2(f: A => Boolean): Option[A] = {
    this flatMap (a => if (f(a)) Some(a) else None)
  }
}

case class Some[A](value: A) extends Option[A]
case object None extends Option[Nothing]


sealed trait Either [+E, +A] {
  def map [B] (f: A => B) : Either [E, B] = this match {
    case Left(e) => Left(e)
    case Right(v) => Right(f(v))
  }

  def flatMap [EE >: E, B] (f: A => Either[EE,B]) : Either[EE,B] = this match {
    case Left(e) => Left(e)
    case Right(v) => f(v)
  }

  def filter [EE >: E] (f : A => Boolean, e : => EE) : Either[EE, A] =
    this flatMap(a => if (f(a)) Right(a) else Left(e))
}

case class Left[E] (value : E) extends Either[E, Nothing]
case class Right[A] (value : A) extends Either[Nothing, A]


object Either {

  def traverse [A, B, E] (as : List[A]) (f : A => Either[E, B]) : Either[E, List[B]] = {
    as.foldRight(Right(Nil) : Either[E, List[B]])((a, b) => b flatMap (bb => f(a) map (aa => aa :: bb)))
  }

  def sequence [A,E] (as : List[Either[E, A]]) : Either [E, List[A]] = traverse (as) (x => x)
}




object Chapter4 {

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty)
      None
    else
      Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (m => mean(xs map (x => math.pow(x - m, 2))))
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def lift2[A, B, C](f: (A, B) => C): (Option[A], Option[B]) => Option[C] =
    (a: Option[A], b: Option[B]) => a map (aa => b map (f curried aa)) getOrElse None

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = lift2(f)(a, b)

  def lift2Alternative[A, B, C](f: (A, B) => C): (Option[A], Option[B]) => Option[C] = (a, b) => map2(a, b)(f)

  def Try[A](a: => A): Option[A] = {
    try {
      Some(a)
    } catch {
      case e: Exception => None
    }
  }

  def sequence [A] (os : List[Option[A]]) : Option[List[A]] = {
    os.foldRight(Some(Nil) : Option[List[A]])((a,b) => b flatMap (bb => a map (aa => aa :: bb)))
  }

  def traverse [A, B] (as : List[A]) (f : A => Option[B]) : Option[List[B]] = {
    as.foldRight(Some(Nil) : Option[List[B]])((a,b) => b flatMap (bb => f(a) map (aa => aa :: bb)))
  }

  def sequence2 [A] (os : List[Option[A]]) : Option[List[A]] = traverse (os) (a => a)
}

