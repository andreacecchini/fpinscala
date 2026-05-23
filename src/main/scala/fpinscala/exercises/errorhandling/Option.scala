package fpinscala.exercises.errorhandling

// Hide std library `Option` since we are writing our own in this chapter
import scala.{Option as _, Some as _, None as _}
import fpinscala.answers.datastructures.List.map

enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] = 
    flatMap(a => Some(f(a)))
  

  def getOrElse[B>:A](default: => B): B = this match
    case Some(get) => get
    case _ => default
  

  def flatMap[B](f: A => Option[B]): Option[B] = this match
    case Some(get) => f(get)
    case _ => None
  

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match
    case Some(_) => this
    case _ => ob
  

  def filter(f: A => Boolean): Option[A] = 
    flatMap(a => if f(a) then Some(a) else None)
  

object Option:

  def failingFn(i: Int): Int =
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try
      val x = 42 + 5
      x + y
    catch case e: Exception => 43 // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.

  def failingFn2(i: Int): Int =
    try
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    catch case e: Exception => 43

  def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = 
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x-m, 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = 
    a.flatMap(a => b.map(b => f(a, b)))

  // Some(1) :: Some(2) :: Some(3) => Some(1 :: 2 :: 3)
  // extension [A] (l: List[A])
  //   def myFoldLeft[B](acc: B)(f: (B, A) => B): B = l match
  //     case head :: next => myFoldLeft(f(acc, head))(f)
  //     case Nil => acc
    
  //   def myFoldRight[B](init: B)(f: (A, B) => B): B = l match
  //     case head :: next => f(head, next.myFoldRight(init)(f))
  //     case Nil => init
    
  def sequence[A](as: List[Option[A]]): Option[List[A]] = 
    // as match
    //   case head :: next => map2(head, sequence(next))(_ :: _)
    //   case Nil => Some(List())
    as.foldRight(Some(List[A]()))((e, acc) => map2(e, acc)(_ :: _))
  
  @main def testSequence(): Unit = 
    println:
      sequence(List())  // Some(Nil)
    println:
      sequence(List(Some(1))) // Some(1 :: Nil)
    println:
      sequence(List(Some(1), Some(2), Some(3))) // Some(1 :: 2 :: 3 :: Nil)
    println:
      sequence(List(Some(1), None, Some(3))) // None

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = 
    sequence(as.map(f(_)))
