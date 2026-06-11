package fpinscala.exercises.laziness

import fpinscala.answers.datastructures.Tree.size
import fpinscala.answers.testing.Gen.maxProp
import fpinscala.exercises.laziness.LazyList.empty
import fpinscala.answers.datastructures.List.startsWith

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = 
    foldRight(List.empty)(_ :: _)
  
  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 0  => LazyList.cons(h(), t().take(n-1))
    case _                    => LazyList.empty
  
  def drop(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 0  => t().drop(n-1)
    case Cons(h, t)           => Cons(h, t)
    case Empty                => LazyList.empty
  
  def takeWhile(p: A => Boolean): LazyList[A] = this match
    case Cons(h, t) if p(h())   => LazyList.cons(h(), t().takeWhile(p))
    case _                      => LazyList.empty

  def forAll(p: A => Boolean): Boolean = 
    foldRight(true)(p(_) && _)
  

  def headOption: Option[A] = this match
    case Cons(h, _) => Some(h())
    case _          => None 
  

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): LazyList[B] = 
    flatMap(a => LazyList.cons(f(a), empty))

  def filter(p: A => Boolean): LazyList[A] =
    flatMap(a => if p(a) then LazyList.cons(a, empty) else empty)

  def append[B >: A](second: LazyList[B]): LazyList[B] = 
    foldRight(second)(LazyList.cons(_, _))
  
  def flatMap[B](f: A => LazyList[B]): LazyList[B] = 
    foldRight(LazyList.empty)(f(_) append _)

  def startsWith[B](s: LazyList[B]): Boolean = ???


object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = LazyList.cons(a, continually(a))

  def from(n: Int): LazyList[Int] = 
    LazyList.cons(n, from(n+1))

  lazy val fibs: LazyList[Int] = 
    ???
    
  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = ???

  lazy val fibsViaUnfold: LazyList[Int] = ???

  def fromViaUnfold(n: Int): LazyList[Int] = ???

  def continuallyViaUnfold[A](a: A): LazyList[A] = ???

  lazy val onesViaUnfold: LazyList[Int] = ???
