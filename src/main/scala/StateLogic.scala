// Блок 2: Writer для логирования операций
// Блок 3: State для изменения состояния банкомата

// Пополнение счета пользователя
def deposit(userId: UserId, amount: Money): State[AtmState, Writer[Unit]] = State { state =>
  if (amount <= 0) {
    val log = List(s"Попытка пополнения на неверную сумму: $amount")
    (state, Writer((log, ())))
  } else {
    val currentBalance = state.balances.getOrElse(userId, 0)
    val newBalances = state.balances.updated(userId, currentBalance + amount)
    val log = List(s"Пополнение счета $userId на сумму $amount")
    (state.copy(balances = newBalances), Writer((log, ())))
  }
}

// Снятие наличных
def withdraw(userId: UserId, amount: Money, plan: List[Int]): State[AtmState, Writer[Either[AtmError, Unit]]] = State { state =>
  val balance = state.balances.getOrElse(userId, 0)
  val alreadyToday = state.withdrawnToday.getOrElse(userId, 0)
  val dayLimit = 10000

  // Проверка возможности выдать план из доступных купюр
  def canIssuePlan(plan: List[Int]): Boolean = {
    plan.groupBy(identity).forall { case (denom, count) =>
      state.cashInAtm.getOrElse(denom, 0) >= count.size
    }
  }

  if (amount <= 0) {
    val log = List(s"Попытка снятия: $amount", "Отказ: Неверная сумма")
    (state, Writer((log, Left(InvalidAmount))))
  } else if (balance < amount) {
    val log = List(s"Попытка снятия: $amount", "Отказ: Недостаточно средств")
    (state, Writer((log, Left(InsufficientBalance))))
  } else if (alreadyToday + amount > dayLimit) {
    val log = List(s"Попытка снятия: $amount", "Отказ: Превышен дневной лимит")
    (state, Writer((log, Left(DayLimitExceeded))))
  } else if (!canIssuePlan(plan)) {
    val log = List(s"Попытка снятия: $amount", "Отказ: Нет нужных купюр в банкомате")
    (state, Writer((log, Left(NoCashForWithdraw))))
  } else {
    val newBalances = state.balances.updated(userId, balance - amount)
    val newWithdrawnToday = state.withdrawnToday.updated(userId, alreadyToday + amount)
    val finalCash = plan.foldLeft(state.cashInAtm) { (cash, denom) =>
      cash.updated(denom, cash.getOrElse(denom, 0) - 1)
    }

    val newState = state.copy(
      balances = newBalances,
      cashInAtm = finalCash,
      withdrawnToday = newWithdrawnToday
    )
    val log = List(s"Снятие: $amount руб.", s"Купюры: ${plan.mkString(", ")}")
    (newState, Writer((log, Right(()))))
  }
}

// Перевод средств между пользователями
def transfer(from: UserId, to: UserId, amount: Money, fee: Money): State[AtmState, Writer[Either[AtmError, Unit]]] = State { state =>
  val fromBalance = state.balances.getOrElse(from, 0)
  val totalCost = amount + fee

  if (amount <= 0) {
    val log = List(s"Неверная сумма перевода: $amount")
    (state, Writer((log, Left(InvalidAmount))))
  } else if (fromBalance < totalCost) {
    val log = List(s"Перевод $from -> $to: $amount (комиссия $fee) не удался: недостаточно средств")
    (state, Writer((log, Left(InsufficientBalance))))
  } else {
    val newBalances = state.balances
      .updated(from, fromBalance - totalCost)
      .updated(to, state.balances.getOrElse(to, 0) + amount)

    val newState = state.copy(balances = newBalances)
    val log = List(s"Перевод: $from -> $to, сумма: $amount, комиссия: $fee")
    (newState, Writer((log, Right(()))))
  }
}

// Сброс дневного лимита (новый день)
def nextDay: State[AtmState, Writer[Unit]] = State { state =>
  val newState = state.copy(withdrawnToday = Map.empty)
  val log = List("Новый день: дневной лимит снятий сброшен")
  (newState, Writer((log, ())))
}