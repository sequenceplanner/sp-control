package sp.abilityhandler

// TODO 180228: Clean up and update this state handler!

// The various states that an ability can be in
object AbilityStatus {
  val Unavailable = "unavailable"
  val NotEnabled = "notEnabled"
  val Enabled = "enabled"
  val Starting = "starting"
  val Executing = "executing"
  val Finished = "finished"
  val ForcedReset = "forcedReset"
  val Failed = "failed"
}