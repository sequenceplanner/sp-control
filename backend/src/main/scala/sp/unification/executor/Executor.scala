package sp.unification

import sp.drivers.ROSFlatStateDriver
import sp.modelSupport._



class Executor extends ModelDSL with ROSSupport {
  import sp.unification.UnificationModel._

  reader("Executor", "unification_roscontrol/ExecutorToSP", "/unification_roscontrol/executor_to_sp")
  writer("Executor", "unification_roscontrol/SPToExecutor", "/unification_roscontrol/sp_to_executor", 250)

  // abilities
  executorCmd.foreach(cmd =>
    a(cmd, List(),
      c("pre", "!done", s"cmd := $cmd"),
      c("started", s"got_cmd == $cmd"),
      c("post", "done"),
      c("reset", "true", s"cmd := ''"))
  )

  a("reset", List(),
    c("pre", "true", s"cmd := ''"),
    c("started", "true"),
    c("post", "true"),
    c("reset", "true")
  )

  driver("Executor", ROSFlatStateDriver.driverType)
  // blank list of things = take everything
  resource("resource")
}
