type UserId = String
type Money = Int

case class AtmConfig(
                      dayLimit: Money,
                      transferFeePercent: Double,
                      availableDenominations: List[Int]
                    )

case class AtmState(
                     balances: Map[UserId, Money],
                     cashInAtm: Map[Int, Int],
                     withdrawnToday: Map[UserId, Money]
                   )

sealed trait AtmError
case object InsufficientBalance extends AtmError
case object DayLimitExceeded extends AtmError
case object NoCashForWithdraw extends AtmError
case object InvalidAmount extends AtmError
case object SelfTransfer extends AtmError