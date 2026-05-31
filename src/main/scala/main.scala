import IO.{RunSync, println, readLine}

object Main {

  // Начальное состояние банкомата
  def initialState: AtmState = AtmState(
    balances = Map("Alice" -> 5000, "Bob" -> 3000),
    cashInAtm = Map(5000 -> 2, 2000 -> 5, 1000 -> 10, 500 -> 20, 100 -> 50),
    withdrawnToday = Map.empty
  )

  // Конфигурация банкомата
  val config: AtmConfig = AtmConfig(
    dayLimit = 10000,
    transferFeePercent = 1.5,
    availableDenominations = List(5000, 2000, 1000, 500, 100)
  )

  // Печать чека
  def printReceipt(log: List[String]): IO[Unit] =
    IO.println("=" * 40).flatMap { _ =>
      IO.println("ЧЕК:").flatMap { _ =>
        def printLines(lines: List[String]): IO[Unit] = lines match {
          case Nil => IO.pure(())
          case head :: tail => IO.println(s"  $head").flatMap(_ => printLines(tail))
        }

        printLines(log).flatMap(_ => IO.println("=" * 40))
      }
    }

  // Выбор пользователя
  def selectUser: IO[UserId] =
    IO.println("Доступные пользователи: Alice, Bob").flatMap { _ =>
      IO.println("Введите имя: ").flatMap { _ =>
        readLine.flatMap { name =>
          if (name == "Alice" || name == "Bob") IO.pure(name)
          else {
            IO.println("Неверное имя, попробуйте снова").flatMap(_ => selectUser)
          }
        }
      }
    }

  // Чтение числа из консоли
  def readInt(prompt: String): IO[Int] =
    IO.println(prompt).flatMap { _ =>
      readLine.flatMap { line =>
        line.toIntOption match {
          case Some(value) => IO.pure(value)
          case None => IO.println("Ошибка: введите число").flatMap(_ => readInt(prompt))
        }
      }
    }

  // Обработка пополнения
  def handleDeposit(userId: UserId, state: AtmState): IO[AtmState] =
    readInt("Введите сумму: ").flatMap { amount =>
      val (newState, writer) = deposit(userId, amount).run(state)
      val (log, _) = writer.run
      printReceipt(log).map(_ => newState)
    }

  // Обработка снятия
  def handleWithdraw(userId: UserId, state: AtmState): IO[AtmState] =
    readInt("Введите сумму: ").flatMap { amount =>
      val planOpt = withdrawPlan(amount).run(config)
      val canWithdrawCheck = canWithdraw(
        state.balances.getOrElse(userId, 0),
        state.withdrawnToday.getOrElse(userId, 0),
        amount
      ).run(config)

      (planOpt, canWithdrawCheck) match {
        case (Some(plan), true) =>
          val (newState, writer) = withdraw(userId, amount, plan).run(state)
          val (log, _) = writer.run
          printReceipt(log).map(_ => newState)

        case _ =>
          val log = List(s"Попытка снятия: $amount", "Отказ: Невозможно обработать")
          printReceipt(log).map(_ => state)
      }
    }

  // Обработка перевода
  def handleTransfer(userId: UserId, state: AtmState): IO[AtmState] =
    IO.println("Введите имя получателя (Alice/Bob): ").flatMap { _ =>
      readLine.flatMap { toUser =>
        // Проверка: нельзя переводить самому себе
        if (toUser == userId) {
          IO.println("Ошибка: нельзя перевести деньги самому себе").flatMap { _ =>
            IO.println("Нажмите Enter для продолжения...").flatMap { _ =>
              readLine.map(_ => state)
            }
          }
        } else if (toUser != "Alice" && toUser != "Bob") {
          IO.println("Ошибка: такого пользователя не существует").flatMap { _ =>
            IO.println("Нажмите Enter для продолжения...").flatMap { _ =>
              readLine.map(_ => state)
            }
          }
        } else {
          readInt("Введите сумму: ").flatMap { amount =>
            val fee = transferFee(amount).run(config)
            val (newState, writer) = transfer(userId, toUser, amount, fee).run(state)
            val (log, _) = writer.run
            printReceipt(log).map(_ => newState)
          }
        }
      }
    }

  // Обработка сброса дня
  def handleNextDay(state: AtmState): IO[AtmState] = {
    val (newState, writer) = nextDay.run(state)
    val (log, _) = writer.run
    printReceipt(log).map(_ => newState)
  }

  // Основной цикл
  def program(state: AtmState): IO[Unit] =
    selectUser.flatMap { userId =>
      IO.println("Операции: deposit, withdraw, transfer, nextDay, exit").flatMap { _ =>
        readLine.flatMap {
          case "exit" =>
            IO.println("До свидания!")

          case "nextDay" =>
            handleNextDay(state).flatMap(program)

          case "deposit" =>
            handleDeposit(userId, state).flatMap(program)

          case "withdraw" =>
            handleWithdraw(userId, state).flatMap(program)

          case "transfer" =>
            handleTransfer(userId, state).flatMap(program)

          case _ =>
            IO.println("Неизвестная команда").flatMap(_ => program(state))
        }
      }
    }

  def main(args: Array[String]): Unit = {
    demoTagless() // демонстрация tagless final
    RunSync(program(initialState))
  }
}