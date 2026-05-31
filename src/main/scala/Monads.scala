// Блок 0: Свои реализации монад

trait Monad[M[_]] {
  def pure[A](a: A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => pure(f(a)))
}

case class Reader[Env, A](run: Env => A) {
  def map[B](f: A => B): Reader[Env, B] = Reader(env => f(run(env)))
  def flatMap[B](f: A => Reader[Env, B]): Reader[Env, B] = Reader(env => f(run(env)).run(env))
}

object Reader {
  def ask[Env]: Reader[Env, Env] = Reader(env => env)
}

case class Writer[A](run: (List[String], A)) {
  def map[B](f: A => B): Writer[B] = {
    val (log, a) = run
    Writer((log, f(a)))
  }
  def flatMap[B](f: A => Writer[B]): Writer[B] = {
    val (log1, a) = run
    val (log2, b) = f(a).run
    Writer((log1 ++ log2, b))
  }
}

object Writer {
  def tell(log: List[String]): Writer[Unit] = Writer((log, ()))
  def pure[A](a: A): Writer[A] = Writer((Nil, a))
}

case class State[S, A](run: S => (S, A)) {
  def map[B](f: A => B): State[S, B] = State(s => {
    val (s2, a) = run(s)
    (s2, f(a))
  })
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (s2, a) = run(s)
    f(a).run(s2)
  })
}

object State {
  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => (s, ()))
  def modify[S](f: S => S): State[S, Unit] = State(s => (f(s), ()))
}

case class IO[A](run: () => A) {
  def map[B](f: A => B): IO[B] = IO(() => f(run()))
  def flatMap[B](f: A => IO[B]): IO[B] = IO(() => f(run()).run())
}

object IO {
  def pure[A](a: A): IO[A] = IO(() => a)
  def println(s: String): IO[Unit] = IO(() => Predef.println(s))
  def readLine: IO[String] = IO(() => scala.io.StdIn.readLine())
  def RunSync[A](io: IO[A]): A = io.run()
}