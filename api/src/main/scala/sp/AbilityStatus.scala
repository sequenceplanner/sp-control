package sp

// TODO 180228: Clean up and update this state handler!

sealed trait AbilityStatus {
  def tag: String
}

// The various states that an ability can be in
object AbilityStatus {
  val UnavailableTag = "unavailable"
  val NotEnabledTag = "notEnabled"
  val EnabledTag = "enabled"
  val StartingTag = "starting"
  val ExecutingTag = "executing"
  val FinishedTag = "finished"
  val ForcedResetTag = "forcedReset"
  val FailedTag = "failed"

  case object Unavailable extends AbilityStatus { val tag: String = UnavailableTag }
  case object NotEnabled extends AbilityStatus { val tag: String = NotEnabledTag }
  case object Enabled extends AbilityStatus { val tag: String = EnabledTag }
  case object Starting extends AbilityStatus { val tag: String = StartingTag }
  case object Executing extends AbilityStatus { val tag: String = ExecutingTag }
  case object Finished extends AbilityStatus { val tag: String = FinishedTag }
  case object ForcedReset extends AbilityStatus { val tag: String = ForcedResetTag }
  case object Failed extends AbilityStatus { val tag: String = FailedTag }

  def fromString(tag: String): Option[AbilityStatus] = tag match {
    case UnavailableTag => Some(Unavailable)
    case NotEnabledTag => Some(NotEnabled)
    case EnabledTag =>Some(Enabled)
    case StartingTag => Some(Starting)
    case ExecutingTag => Some(Executing)
    case FinishedTag => Some(Finished)
    case ForcedResetTag => Some(ForcedReset)
    case FailedTag => Some(Failed)
    case _ => None
  }

  implicit def asString(abilityStatus: AbilityStatus): String = abilityStatus.tag
}