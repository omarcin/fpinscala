package com.oczeretko

import scala.util.matching.Regex

trait Parsers[Parser[+_]] {
  self =>

  /* Types */
  case class ParseError(stack: List[(String, Location)])

  /* execution */
  def run[A](p: Parser[A])(text: String): Either[ParseError, A]

  /* ctors */
  def string(s: String): Parser[String]

  def regex(r: Regex): Parser[String]

  val whitespace = regex("\\s".r).label("Expected whitespace character")
  val double: Parser[Double] =
    (
      char('-').? **
      digit.+ **
      (
        (char(',') | char('.')) **
        digit.+
      ).?
    ).slice map (_.toDouble)

  val digit = regex("\\d".r).label("Expected digit")

  def char(a: Char): Parser[Char] = string(a.toString).map(_.charAt(0))

  def succeed[A](a: A): Parser[A]

  def exceptChar(a: Char): Parser[Char] = regex(("[^" + a + "]").r).map(_.charAt(0))

  /* operators */
  def as[A, B](p: Parser[A], b: B): Parser[B] = p.map(_ => b)

  def or[A](left: Parser[A], right: => Parser[A]): Parser[A]

  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    for {
      a <- p1
      b <- p2
    } yield (a, b)

  def attempt[A](p: Parser[A]): Parser[A]

  def listOf[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List.empty[A])
    else map2(p, listOf(n - 1, p))(_ :: _)

  def maybe[A](p: Parser[A]) : Parser[Option[A]] = p.map(Some(_)) | succeed(None)

  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) | succeed(List.empty[A])

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  def trim[A](p: Parser[A]): Parser[A] =
    whitespace.* *--
      p **
      whitespace.* *--

  def separated[A, B](sep: Parser[A], pb: Parser[B]): Parser[List[B]] =
    ((
      pb **
        sep *--
      ).* **
      pb map { case (lb, b) => lb ++ List(b)}) |
      succeed(List.empty[B]) scope ("Expected separated list of values")

  def ignoreFirst[A, B](p1: Parser[A], p2: Parser[B]): Parser[B] = p1 ** p2 map (_._2)

  def ignore[A, B, C](p1: Parser[(A, B)], p2: Parser[C]): Parser[(A, C)] = p1 ** p2 map { case ((a, b), c) => (a, c)}

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = flatMap(p)(a => succeed(f(a)))

  def map2[A, B, C](pa: Parser[A], pb: => Parser[B])(f: (A, B) => C): Parser[C] =
    for {
      a <- pa
      b <- pb
    } yield f(a, b)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def slice[A](p: Parser[A]): Parser[String]

  /* error reporting */

  def debug[A](p: Parser[A])(msg: String): Parser[A]

  def label[A](p: Parser[A])(msg: String): Parser[A]

  def scope[A](p: Parser[A])(msg: String): Parser[A]

  case class Location(input: String, offset: Int) {
    lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val column = input.slice(0, offset + 1).lastIndexOf('\n') match {
      case -1 => offset + 1
      case lineStart => offset - lineStart
    }
  }

  /* Conversions */
  implicit def toBiParserOps[A, B](a: Parser[(A, B)]): BiParserOps[A, B] = BiParserOps(a)

  implicit def fromBiParserOps[A, B](a: BiParserOps[A, B]): Parser[(A, B)] = a match {
    case BiParserOps(p) => p
  }

  implicit def toParserOps[A](a: Parser[A]): ParserOps[A] = ParserOps(a)

  implicit def fromParserOps[A](a: ParserOps[A]): Parser[A] = a match {
    case ParserOps(p) => p
  }

  implicit def toParser[A](a: A)(implicit aToString: A => String) = string(aToString(a))

  implicit def toParser(r: Regex) = regex(r)

  case class BiParserOps[A, B](p: Parser[(A, B)]) {
    def *-- : Parser[A] = p.map(_._1)

    def *--[C](p2: Parser[C]): Parser[(A, C)] = p ** p2 map { case ((a, b), c) => (a, c)}
  }

  /* Operators */
  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def **[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def *--[B](p2: Parser[B]): Parser[B] = self.ignoreFirst(p, p2)

    def as[B](b: B): Parser[B] = self.as(p, b)

    def slice: Parser[String] = self.slice(p)

    def trim: Parser[A] = self.trim(p)

    def map[B](f: A => B) = self.map(p)(f)

    def map2[B, C](pb: => Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, pb)(f)

    def maybe: Parser[Option[A]] = self.maybe(p)
    def ? : Parser[Option[A]] = self.maybe(p)

    def many: Parser[List[A]] = self.many(p)
    def * : Parser[List[A]] = self.many(p)

    def many1: Parser[List[A]] = self.many1(p)
    def + : Parser[List[A]] = self.many1(p)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def label(msg: String): Parser[A] = self.label(p)(msg)

    def debug(msg: String): Parser[A] = self.debug(p)(msg)

    def scope(msg: String): Parser[A] = self.scope(p)(msg)
  }

  object Laws {

    def equal[A](p1: Parser[A], p2: Parser[A])(in: SGen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: SGen[String]): Prop =
      equal(p.map(a => a), p)(in)

    def testDouble : Prop = {
      Prop.forAll(SGen.double)(input => run(self.double)(input.toString) match {
        case Right(num) => num == input
        case _ => false
      })
    }

    def testValidJsonString (p: Parser[JSON]) : Prop = {
      Prop.forAll(SGen.string.map("'" + _ + "'"))(input => {
        run(p)(input) match {
          case Right(JSON.JSString(input)) => true
          case Right(_) => throw new Exception("Expected JSString")
          case Left(pe) => throw new Exception(pe.stack.toString())
        }
      })
    }

    def testValidJson (p: Parser[JSON]): Prop = {
      val testJson = "  { 'Name' = 'Marcin', 'Age'=15, Items=[11,2,3] } "
      Prop.check({
        val result = run(p)(testJson)
        result match {
          case Right(JSON.JSObject(map)) =>
            map.get("Name").get.asInstanceOf[JSON.JSString].get == "Marcin" &&
              map.get("Age").get.asInstanceOf[JSON.JSNumber].get == 15 &&
              map.get("Items").get.asInstanceOf[JSON.JSArray].get.head.asInstanceOf[JSON.JSNumber].get == 11
          case Right(_) => throw new Exception("Expected JSObject")
          case Left(pe) => throw new Exception(pe.stack.toString())
        }
      })
    }
  }
}

trait JSON

object JSON {
  case object JSNull extends JSON
  case class JSNumber(get: Double) extends JSON
  case class JSString(get: String) extends JSON
  case class JSBool(get: Boolean) extends JSON
  val JSTrue = JSBool(true)
  val JSFalse = JSBool(false)
  case class JSArray(get: IndexedSeq[JSON]) extends JSON
  case class JSObject(get: Map[String, JSON]) extends JSON
}

object Parsers {
  import JSON._

  def jsnull [Parser[+_]](P : Parsers[Parser]) : Parser[JSNull.type] = {
    import P._
    string("null").as(JSNull)
  }

  def jsnumber [Parser[+_]](P : Parsers[Parser]) : Parser[JSNumber] = {
    import P._
    double.map(JSNumber)
  }

  def jsstring [Parser[+_]](P : Parsers[Parser]) : Parser[JSString] = {
    import P._
    for {
      p <- char('\'') | char('"')
      str <- exceptChar(p).*.slice
      _ <- char(p)
    } yield JSString(str)
  }

  def jsbool [Parser[+_]](P : Parsers[Parser]) : Parser[JSBool] = {
    import P._
    string("false").as(JSFalse) |
    string("true").as(JSTrue)
  }

  def jsarray [Parser[+_]](P : Parsers[Parser]): Parser[JSArray] = {
    import P._
    (
      char('[') *--
      separated(char(','), jsonParser(P)).trim **
      char(']') *--
    ) map (_.toIndexedSeq) map (JSArray) scope ("JSON Array Expected")
  }

  def jsobject [Parser[+_]](P : Parsers[Parser]): Parser[JSObject] = {
    import P._

    (
      char('{') *--
        separated(
          char(','),
          (
            jsstring(P) **
            char('=').trim *--
            jsonParser(P)
            ).trim
        ).trim **
        char('}') *--
      ) map (_.map(p => (p._1.get, p._2)).toMap) map (JSObject) scope ("JSON Object Expected")
  }

  def jsonParser[Parser[+_]](P : Parsers[Parser]) : Parser[JSON] = {
    import P._
    (jsnull(P) | jsnumber(P) | jsbool(P) | jsstring(P) | jsarray(P) | jsobject(P)).trim scope ("JSON expected")
  }
}

case class MyParser[+A] (parse: String => Either[MyParsers.ParseError, (A, String)])

object MyParsers extends Parsers[MyParser] {
  override def run[A](p: MyParser[A])(input: String): Either[MyParsers.ParseError, A] = p.parse(input).right.map(_._1)

  override def flatMap[A, B](p: MyParser[A])(f: (A) => MyParser[B]): MyParser[B] = {
    MyParser(input => p.parse(input) match {
      case Left(error) => Left(error)
      case Right((a, rest)) => f(a).parse(rest)
      })
  }

  override def string(s: String): MyParser[String] = MyParser(input => {
    if (input.startsWith(s))
      Right(s, input.slice(s.length, input.length))
    else
      Left(ParseError(List.empty))
  }).label("Expected " + s)

  override def scope[A](p: MyParser[A])(msg: String): MyParser[A] = p

  override def regex(r: Regex): MyParser[String] = MyParser(input =>
    r.findPrefixMatchOf(input)
     .map(m => Right(m.matched, input.slice(m.end, input.length)))
     .getOrElse(Left(ParseError(List.empty)))
  ) label("Regex " + r)

  override def slice[A](p: MyParser[A]): MyParser[String] = MyParser(input => {
    p.parse(input).right.map { case (_, rest) => (input.slice(0, input.length - rest.length), rest) }
  })

  override def label[A](p: MyParser[A])(msg: String): MyParser[A] = p

  override def attempt[A](p: MyParser[A]): MyParser[A] = p

  override def or[A](left: MyParser[A], right: => MyParser[A]): MyParser[A] = MyParser(input => {
    left.parse(input) match {
      case Left(errorLeft) => right.parse(input) match {
        case Left(errorRight) => Left(ParseError(errorLeft.stack ++ errorRight.stack))
        case r => r
      }
      case r => r
    }
  })

  override def succeed[A](a: A): MyParser[A] = MyParser(input => Right(a, input))

  override def debug[A](p: MyParser[A])(msg: String): MyParser[A] = MyParser(input => {
    Console.println(msg)
    p.parse(input)
  })
}

object ParserMain {
  def main(): Unit = {
    import MyParser._

    val jsonParser = Parsers.jsstring(MyParsers)
    Console.println("mapLaw")
    Prop.run(MyParsers.Laws.mapLaw(jsonParser)(SGen.string))
    Console.println("double")
    Prop.run(MyParsers.Laws.testDouble)
    Console.println("jsstring")
    Prop.run(MyParsers.Laws.testValidJsonString(jsonParser))
    Console.println("json")
    Prop.run(MyParsers.Laws.testValidJson(jsonParser))
  }
}