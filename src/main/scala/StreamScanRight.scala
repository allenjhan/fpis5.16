object StreamScanRight extends App{
  println(Stream(1,2,3).scanRight(0)(_ + _).toList)
  println(Stream(1,2,3).inefficientScanRight(0)(_ + _).toList)

}


import Stream._
trait Stream[+A] {

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = {
    unfold((n, this): (Int, Stream[A])) { case (i, stream) =>
      if (i <= 0) None
      else {
        stream match {
          case Cons(h, t) => Some(h(), (i-1, t()))
          case Empty => None
        }
      }
    }
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case s @ Cons(h, t) if n == 0 => s
    case Cons(h, t) => t().drop(n-1)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    unfold(this){ stream =>
      stream match {
        case Cons(h, t) if p(h()) => Some((h(), t()))
        case _ => None
      }
    }
  }

  def forAll(p: A => Boolean): Boolean = {
    this.foldRight(true){ (e, acc) =>
      acc && p(e)
    }
  }

  def headOption: Option[A] = {
    this.foldRight(None: Option[A]){(e, acc) =>
      Some(e)
    }
  }

  def map[B](f: A => B): Stream[B] = unfold(this){in =>
    in match {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    }
  }

  def filter(f: A => Boolean): Stream[A] = this.foldRight(Empty:Stream[A])((e, acc)=>if(f(e)) Cons(()=>e, ()=>acc) else acc)

  def flatMap[B](f: A => Stream[B]): Stream[B] = this.foldRight(Empty:Stream[B]){(e, acc)=>
    f(e).foldRight(acc)((e1, acc1)=>Cons(()=>e1,()=>acc1))
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = {
    unfold((this, s2)) { case (stream1, stream2) =>
      val (v1, state1) = stream1 match {
        case Cons(h, t) => (Some(h()), t())
        case Empty => (None, Empty)
      }
      val (v2, state2) = stream2 match {
        case Cons(h, t) => (Some(h()), t())
        case Empty => (None, Empty)
      }
      if (v1.isEmpty && v2.isEmpty) None
      else Some((v1, v2), (state1, state2))
    }
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = {
    val zipFilter = this.zipAll(s).takeWhile{case (aOpt, bOpt) => aOpt.isDefined && bOpt.isDefined}.filter{case (aOpt, bOpt) => aOpt != bOpt}

    zipFilter match {
      case Cons(_, _) => false
      case Empty => true
    }
  }

  def tails: Stream[Stream[A]] = {
    unfold(Some(this): Option[Stream[A]]){ streamOpt =>
      streamOpt.map { s =>
        s match {
          case c @ Cons(h, t) => (c, Some(t()))
          case Empty => (empty, None)
        }
      }
    }
  }

  def inefficientScanRight[S](z:S)(f:(A, => S)=>S): Stream[S] = {
    this.tails.map{_.foldRight(z)(f)}
  }

  def scanRight[S](z:S)(f:(A, => S)=>S): Stream[S] = {
    this.foldRight(Stream(z):Stream[S]){
      case (e, acc) =>
        acc match {
          case c @ Cons(h, t) => Cons(() => f(e, h()), () => c)
          case Empty => Cons(()=>z, ()=>Empty)
        }
    }
  }

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = unfold(1)(s => Some(1, 1))
  def constant(n: Int): Stream[Int] = unfold(n)(s => Some((n, n)))
  def from(n: Int): Stream[Int] = unfold(n)(s=>Some((s,s+1)))
  def fibs: Stream[Int] = unfold((-2,0,-1,0):Tuple4[Int,Int,Int,Int])(s =>
    if(s._3==0) Some(1, (0,0,1,1))
    else Some((s._2+s._4, (s._3,s._4,s._3+1,s._2+s._4)))
  )

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some(v) => Cons(()=>v._1, ()=>unfold(v._2)(f))
    case None => Empty
  }

  def zipWith[A](in1: Stream[A], in2: Stream[A])(f: (A, A)=>A): Stream[A] = {
    unfold((in1, in2)){ case (stream1, stream2) =>
      stream1 match {
        case Cons(h1, t1) =>
          stream2 match {
            case Cons(h2, t2) => Some((f(h1(), h2()), (t1(), t2())))
            case Empty => None
          }
        case Empty => None
      }
    }
  }
}
