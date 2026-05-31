// Блок 1: Reader - логика с конфигурацией

// Разбиение суммы на купюры (жадно, с учетом доступных купюр)
// Возвращает None если нельзя разбить
def withdrawPlan(amount: Money): Reader[AtmConfig, Option[List[Int]]] = Reader { config =>
  def greedy(remaining: Money, denoms: List[Int]): List[Int] = denoms match {
    case Nil => Nil
    case d :: ds if d <= remaining =>
      d :: greedy(remaining - d, denoms)  // можно использовать эту купюру снова
    case _ :: ds => greedy(remaining, ds)
  }

  val plan = greedy(amount, config.availableDenominations.sorted.reverse)
  if (plan.sum == amount) Some(plan) else None
}

// Комиссия за перевод
def transferFee(amount: Money): Reader[AtmConfig, Money] = Reader { config =>
  (amount * config.transferFeePercent / 100).toInt
}

// Проверка возможности снятия
def canWithdraw(balance: Money, alreadyToday: Money, amount: Money): Reader[AtmConfig, Boolean] = Reader { config =>
  amount > 0 &&
    balance >= amount &&
    alreadyToday + amount <= config.dayLimit
}

// Округление суммы вниз до ближайшей возможной
def roundedAmount(amount: Money): Reader[AtmConfig, Money] = Reader { config =>
  val sortedDenoms = config.availableDenominations.sorted
  val minDenom = if (sortedDenoms.nonEmpty) sortedDenoms.head else 1
  (amount / minDenom) * minDenom
}