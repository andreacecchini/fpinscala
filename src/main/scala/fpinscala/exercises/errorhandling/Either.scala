package fpinscala.exercises.errorhandling

// Hide std library `Either` since we are writing our own in this chapter
import scala.{Either as _, Left as _, Right as _}
import scala.util.control.NonFatal

enum Either[+E,+A]:
  case Left(get: E)
  case Right(get: A)

  def map[B](f: A => B): Either[E, B] = 
    flatMap(a => Right(f(a)))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match
    case Right(a) => f(a)
    case Left(e) => Left(e)  

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match
    case Right(a) => this
    case Left(e) => b

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = 
    for 
      a <- this
      b <- b
    yield f(a, b)

object Either:
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = 
     es.foldRight[Either[E,List[B]]](Right(List()))((a, b) => f(a).map2(b)(_ :: _))

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = 
    traverse(es)(identity)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if xs.isEmpty then
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Throwable, Int] = 
    try Right(x / y)
    catch case NonFatal(t) => Left(t)

  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)

  def map2All[E, A, B, C](a: Either[List[E], A], b: Either[List[E], B], f: (A, B) => C): Either[List[E], C] = 
    (a, b) match
      case (Right(aa), Right(bb)) => Right(f(aa, bb))
      case (Left(e1), Left(e2)) => Left(e1 ++ e2)
      case (Left(e), _) => Left(e)
      case (_, Left(e)) => Left(e)

  def traverseAll[E, A, B](as: List[A], f: A => Either[List[E], B]): Either[List[E], List[B]] = 
    as.foldRight[Either[List[E],List[B]]](Right(List()))((a, b) => map2All(f(a), b, _ :: _))

  def sequenceAll[E, A](as: List[Either[List[E], A]]): Either[List[E], List[A]] = 
    traverseAll(as, identity)

  def lift[E, A](e: Either[E, A]): Either[List[E], A] = e match
    case Right(a) => Right(a)
    case Left(e) => Left(List(e))

  @main def testTraverseAndSequence(): Unit = 
    type Error = String
    case class Age(years: Int)
    object Age:
      def apply(years: Int): Either[Error, Age] = years match
          case y if y >= 0 => Right(new Age(years))
          case _ => Left("Age must be positive")
    case class Name(name: String)
    object Name:
      def apply(name: String): Either[Error, Name] = name match
        case "" | null => Left("Name must not be empty or null")
        case _ => Right(new Name(name)) 
    case class Person(name: Name, age: Age)
    object Person:
      def apply(name: String, age: Int): Either[List[Error], Person] = 
        Either.map2All(lift(Name(name)), lift(Age(age)), Person(_, _))
    val p = Person("Andrea", 24) // Right(Person(Name(Andrea),Age(24)))
    val p1 = Person("", 24) // Left(List(Name must not be empty or null))
    val p2 = Person("Andrea", -1) // Left(List(Age must be positive))
    val p3 = Person("", -1) // Left(List(Name must not be empty or null, Age must be positive))