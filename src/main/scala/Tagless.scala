// Тайп классы (интерфейсы эффектов) описывают, что умеет делать абстрактный тип F

// Базовый интерфейс для связывания операций
trait MyMonad[F[_]] {
  def pure[A](a: A): F[A]                     // поднять значение в контекст
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]  // связать две операции
}

// Интерфейс для работы с состоянием
trait CanState[F[_], S] {
  def get: F[S]
  def set(s: S): F[Unit]
}

// Интерфейс для логирования
trait CanLog[F[_]] {
  def log(msg: String): F[Unit]
}

// Интерфейс для ошибок
trait CanFail[F[_]] {
  def fail[A](err: String): F[A]
}

// Бизнес-логика. Функции не знают, какой F будет использован, они только требуют, чтобы F реализовывал нужные интерфейсы
// Пополнение счета
def depositTagless[F[_]](
                          userId: String,
                          amount: Int
                        )(
                          implicit M: MyMonad[F],           // F должен быть монадой
                          S: CanState[F, AtmState],         // F должен уметь работать с состоянием
                          L: CanLog[F],                     // F должен уметь логировать
                          Err: CanFail[F]                   // F должен уметь обрабатывать ошибки
                        ): F[Unit] = {
  if (amount <= 0) {
    L.log(s"Ошибка: неверная сумма $amount")
  } else {
    M.flatMap(S.get) { state =>
      val current = state.balances.getOrElse(userId, 0)
      val newState = state.copy(balances = state.balances.updated(userId, current + amount))
      M.flatMap(S.set(newState)) { _ =>
        L.log(s"Пополнение $userId на $amount")
      }
    }
  }
}

// Снятие наличных
def withdrawTagless[F[_]](
                           userId: String,
                           amount: Int,
                           plan: List[Int]
                         )(
                           implicit M: MyMonad[F],
                           S: CanState[F, AtmState],
                           L: CanLog[F],
                           Err: CanFail[F]
                         ): F[Unit] = {
  val dayLimit = 10000

  M.flatMap(S.get) { state =>
    val balance = state.balances.getOrElse(userId, 0)
    val already = state.withdrawnToday.getOrElse(userId, 0)

    // Валидация
    if (amount <= 0) Err.fail("Неверная сумма")
    else if (balance < amount) Err.fail("Недостаточно средств")
    else if (already + amount > dayLimit) Err.fail("Превышен дневной лимит")
    else {
      // Успех: обновляем состояние и логируем
      val newState = state.copy(
        balances = state.balances.updated(userId, balance - amount),
        withdrawnToday = state.withdrawnToday.updated(userId, already + amount)
      )
      M.flatMap(S.set(newState)) { _ =>
        L.log(s"Снятие: $amount руб. Купюры: ${plan.mkString(", ")}")
      }
    }
  }
}

// Интерпретатор. Конкретная реализация эффектов для типа MyIO

class MyRuntime(initial: AtmState) {
  // Внутреннее состояние интерпретатора
  private var st: AtmState = initial
  private var msgs: List[String] = Nil
  // Наш конкретный тип эффекта: просто функция () => A
  case class MyIO[A](run: () => A)

  // Реализация MyMonad для MyIO
  implicit val monad: MyMonad[MyIO] = new MyMonad[MyIO] {
    def pure[A](a: A): MyIO[A] = MyIO(() => a)
    def flatMap[A, B](fa: MyIO[A])(f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f(fa.run()).run())
  }

  // Реализация CanState для MyIO
  implicit val state: CanState[MyIO, AtmState] = new CanState[MyIO, AtmState] {
    def get: MyIO[AtmState] = MyIO(() => st)
    def set(s: AtmState): MyIO[Unit] = MyIO(() => { st = s })
  }

  // Реализация CanLog для MyIO
  implicit val log: CanLog[MyIO] = new CanLog[MyIO] {
    def log(msg: String): MyIO[Unit] = MyIO(() => { msgs = msgs :+ msg })
  }

  // Реализация CanFail для MyIO
  implicit val fail: CanFail[MyIO] = new CanFail[MyIO] {
    def fail[A](err: String): MyIO[A] = MyIO(() => {
      msgs = msgs :+ s"ОШИБКА: $err"
      null.asInstanceOf[A]
    })
  }

  def run[A](program: MyIO[A]): (AtmState, List[String]) = {
    program.run()
    (st, msgs)
  }
}

// === Часть 4. Демонстрация ===
def demoTagless(): Unit = {
  // Начальное состояние
  val start = AtmState(
    balances = Map("Alice" -> 1000),
    cashInAtm = Map(500 -> 2),
    withdrawnToday = Map.empty
  )

  val runtime = new MyRuntime(start)

  val program = depositTagless[runtime.MyIO]("Alice", 500)

  // Запускаем программу на интерпретаторе
  val (newState, log) = runtime.run(program)

  // Выводим результат
  println("=== Tagless Final ===")
  println(log.mkString(", "))
  println(s"Баланс Alice: ${newState.balances.getOrElse("Alice", 0)}")
}