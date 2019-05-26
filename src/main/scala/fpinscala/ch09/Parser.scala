package fpinscala.ch09

import scala.util.matching.Regex
import fpinscala.ch08._

trait Parsers[Parser[+_]] { self =>

    def run[A](p: Parser[A])(input: String): Either[ParseError, A]

    def char(c: Char): Parser[Char] =
        string(c.toString).map(_.charAt(0))

    // Laws
    // 1. run(char(c))(c.toString) == Right(c)
    // 2. run(string(s))(s) == Right(s)
    // 3. run(or(string("abra"), string("cadabra")))("abra") == Right("abra")
    //    run(or(string("abra"), string("cadabra")))("cadabra") == Right("cadabra")
    // 4. run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
    //    run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab")
    //    run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab")
    // 5. run(zeroOrMore("a"))("") == Right(0)
    //    run(zeroOrMore("a"))("aa") == Right(2)
    //    run(zeroOrMore("a"))("b123") == Right(0)
    // 6. run(oneOrMore("a"))("aa") == Right(2)
    //    run(oneOrMore("a"))("b123") == Left("Expected one or more 'a'")
    // 7. run(concat(zeroOrMore("a"), oneOrMore("b")))("aabbb") == Right((2, 3))
    // 8. run(traverse(char("a"), 0)((b, a) => b + 1))("aaabbc") == Right(3)
    // 9. map(p)(a => a) == p

    // Exercise 9.2
    // 10. run(product("a", "b"))("abb") == map2(run("a")("abb"), run("b")("abb"))

    implicit def string(s: String): Parser[String]

    def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

    // we need mapping function that converts parser[A] to parser[Int]
    def zeroOrMore[A](parser: Parser[A]): Parser[Int]

    // When we run parser, it should return ParseError with appropriate message
    // We can build this by combining parser that counts all matches for primitive parser
    // and parser that always returns error
    // We need flatMap for this.
    def oneOrMore[A](parser: Parser[A]): Parser[Int]

    // We can build concat by chaining flatMaps with parser that count matches zeroOrMore/oneOrMore
    // and than combining them into tuple
    def concat[A, B](parser1: Parser[A], parser2: Parser[B]): Parser[(Int, Int)]

    // Considerations
    //
    // We probably want to define combinator that is able to go through whole sequence and
    // apply function to chars to accumulate results, we can call it traverse
    //
    // We can use traverse to define repetitions, so repetitions aren't primitive operations
    // in our algebra
    //
    // Our minimal algebra for ParseErrors should provide basic constructor to create ParseError
    // from string. We can define typeclass with one primitive method fromString, that ParseError
    // has to provide or alternatively scala trait that ParseError has to implement.
    // Trait is more limiting because than we can only use types that are under our control, otherwise
    // we cannot extend them
    //
    // a | b should be commutative in our algebra, otherwise or would be inconsistent.
    // if a | b isn't commutative than we change its definition to "a followed by b".
    def traverse[A, B](p: Parser[A], zero: B)(f: (B, A) => B): Parser[B]

    def many[A](parser: Parser[A]): Parser[List[A]]

    // Exercise 9.8
    def map[A, B](parser: Parser[A])(f: A => B): Parser[B] =
        flatMap(parser)(a => succeed(f(a)))

    val numA: Parser[Int] = map(many(char('a')))(_.size)

    def succeed[A](a: A): Parser[A] =
        string("").map(_ => a)

    def product[A, B](p1: Parser[A], p2: Parser[B]): Parser[(A, B)]

    // Exercise 9.1
    def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
        map(product(p1, p2)) {
            case (a, b) => f(a, b)
        }

    def many1[A](p: Parser[A]): Parser[List[A]] =
        map2(p, many(p))((a, as) => a :: as)

    def many_map2[A](p: Parser[A]): Parser[List[A]] =
        or(
            succeed(List[A]()),
            map2(p, lazyUnit(many(p)))((a, as) => a :: as)
        )

    def slice[A](p: Parser[A]): Parser[A]

    // Exercise 9.4
    def listOfN_map2[A](n: Int, p: Parser[A]): Parser[List[A]] =
        List.fill(n)(p).foldRight(succeed(Nil: List[A])) { (a, acc) =>
            map2(a, acc)(_ :: _)
        }

    // Exercise 9.5
    def lazyUnit[A](p: => Parser[A]): Parser[A]

    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

    // Exercise 9.6
    implicit def regex(r: Regex): Parser[String]

    val digits: Parser[Int] = regex("[0-9]+".r).map(_.toInt)

    val digitsAndLetters: Parser[String] =
        flatMap(digits)(d => map(listOfN(d, char('a')))(_.toString))

    // Exercise 9.7
    def product_flatMap[A, B](p1: Parser[A], p2: Parser[B]): Parser[(A, B)] =
        flatMap(p1)(a => map(p2)(b => (a, b)))

    def map2_flatMap[A, B, C](p1: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] =
        flatMap(p1)(a => map(p2)(b => f(a, b)))

    def scope[A](msg: String)(p: Parser[A]): Parser[A]

    def errorLocation(e: ParseError): Location
    def errorMessage(e: ParseError): String

    def attempt[A](p: Parser[A]): Parser[A]

    implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
        ParserOps(f(a))

    object Laws {
        def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
            Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

        def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
            equal(p, p.map(a => a))(in)

        
    }

    case class ParserOps[A](p: Parser[A]) {
        def |[B >: A](p2: => Parser[B]): Parser[B] = or(p2)
        def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
        def map[B](f: A => B): Parser[B] = self.map(p)(f)
        def many[B >: A](): Parser[List[B]] = self.many(p)

        def **[B](p2: Parser[B]): Parser[(A, B)] = product(p2)
        def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

        def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
        def slice[B >: A](): Parser[B] = self.slice(p)
    }
}

case class Location(input: String, offset: Int = 0) {
    lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
        case -1 => offset + 1
        case lineStart => offset - lineStart
    }
}

case class ParseError(stack: List[(Location, String)])
