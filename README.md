# Лабораторная работа №2: Tagless Final

## Вариант 7: Банкомат (ATM)

### Описание

Реализация симуляции работы банкомата с использованием Tagless Final подхода.

В отличие от первой лабораторной работы, где бизнес-логика была жестко привязана к конкретным монадам (Reader, Writer, State), здесь программа абстрагирована от конкретных эффектов через тайп классы.

Tagless final — это подход к написанию программ, при котором:

Бизнес-логика пишется в терминах абстрактных интерфейсов (тайп классов)

Интерпретаторы предоставляют конкретные реализации этих интерфейсов

Программа не знает, какой интерпретатор будет использован

При запуске demoTagless() вы увидите:

Tagless Final 

Пополнение Alice на 500

Баланс Alice: 1500

Tagless final позволяет писать бизнес-логику, не зависящую от конкретных эффектов. Это делает код более гибким, тестируемым и переиспользуемым.

### Тайп классы
- trait MyMonad[F[_]]       // базовый: pure, flatMap
- trait CanState[F[_], S]   // работа с состоянием: get, set
- trait CanLog[F[_]]        // логирование: log
- trait CanFail[F[_]]       // ошибки: fail

### Бизнес-логика
def depositTagless[F[_]](

  userId: String,
  
  amount: Int
  
)(

  implicit M: MyMonad[F],
  
  S: CanState[F, AtmState],
  
  L: CanLog[F],
  
  Err: CanFail[F]
  
): F[Unit] = { ... }

Функция не знает, какой F будет использован. Она только требует, чтобы F поддерживал нужные операции.

### Интерпретатор
class MyRuntime(initial: AtmState) {

  case class MyIO[A](run: () => A)
  
  implicit val monad: MyMonad[MyIO] = ...
  
  implicit val state: CanState[MyIO, AtmState] = ...
  
  implicit val log: CanLog[MyIO] = ...
  
  implicit val fail: CanFail[MyIO] = ...
}

Интерпретатор реализует все тайп классы для конкретного типа MyIO.
