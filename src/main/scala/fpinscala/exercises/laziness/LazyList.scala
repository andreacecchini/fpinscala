package fpinscala.exercises.laziness

import fpinscala.exercises.laziness.LazyList.{empty, unfold}

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] =
    foldRight(List.empty)(_ :: _)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 0 => LazyList.cons(h(), t().take(n - 1))
    case _ => LazyList.empty
  
  def takeViaUnfold(n: Int): LazyList[A] =
    unfold((this, n)):
      case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
      case _ => None

  def drop(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case Cons(h, t) => Cons(h, t)
    case Empty => LazyList.empty

  def takeWhile(p: A => Boolean): LazyList[A] = this match
    case Cons(h, t) if p(h()) => LazyList.cons(h(), t().takeWhile(p))
    case _ => LazyList.empty

  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] =
    unfold(this):
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)(p(_) && _)


  def headOption: Option[A] = this match
    case Cons(h, _) => Some(h())
    case _ => None


  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): LazyList[B] =
    flatMap(a => LazyList.cons(f(a), empty))

  def mapViaUnfold[B](f: A => B): LazyList[B] =
    unfold(this):
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None

  def filter(p: A => Boolean): LazyList[A] =
    flatMap(a => if p(a) then LazyList.cons(a, empty) else empty)

  def append[B >: A](second: LazyList[B]): LazyList[B] =
    foldRight(second)(LazyList.cons(_, _))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(LazyList.empty)(f(_) append _)

  def zipWith[B, C](that: LazyList[B])(f: (A, B) => C): LazyList[C] =
    unfold((this, that)):
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold((this, that)):
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()) -> Some(h2()), (t1(), t2())))
      case (Cons(h1, t1), Empty) => Some((Some(h1()) -> None, (t1(), empty)))
      case (Empty, Cons(h2, t2)) => Some((None -> Some(h2()), (empty, t2())))
      case _ => None

  def startsWith[B](prefix: LazyList[B]): Boolean =
    (this zipAll prefix)
      .takeWhile(_._2.isDefined)
      .forAll { _ == _ }

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail *))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = LazyList.cons(a, continually(a))

  def from(n: Int): LazyList[Int] =
    LazyList.cons(n, from(n + 1))

  lazy val fibs: LazyList[Int] =
    def go(curr: Int, next: Int): LazyList[Int] =
      cons(curr, go(next, curr + next))
    go(0, 1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = f(state) match
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty

  lazy val fibsViaUnfold: LazyList[Int] =
    unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))

  def fromViaUnfold(n: Int): LazyList[Int] =
    unfold((n, n + 1))(s => Some((s._1, (s._2, s._2 + 1))))

  def continuallyViaUnfold[A](a: A): LazyList[A] =
    unfold(a)(a => Some(a, a))

  lazy val onesViaUnfold: LazyList[Int] =
    continuallyViaUnfold(1)
