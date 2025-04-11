import gleam/result

pub fn with_retry(experiment: fn() -> Result(t, e)) -> Result(t, e) {
  case experiment() {
    Ok(t) -> Ok(t)
    Error(_) -> experiment()
  }
}

pub fn record_timing(
  time_logger: fn() -> Nil,
  experiment: fn() -> Result(t, e),
) -> Result(t, e) {
  time_logger()
  let r = experiment()
  time_logger()

  r
}

pub fn run_experiment(
  name: String,
  setup: fn() -> Result(t, e),
  action: fn(t) -> Result(u, e),
  record: fn(t, u) -> Result(v, e),
) -> Result(#(String, v), e) {
  // let a = setup()

  // case a {
  //   Ok(t) -> {
  //     let b = action(t)
  //     case b {
  //       Ok(u) -> {
  //         let c = record(t, u)
  //         case c {
  //           Ok(v) -> Ok(#(name, v))
  //           Error(e) -> Error(e)
  //         }
  //       }
  //       Error(e) -> Error(e)
  //     }
  //   }
  //   Error(e) -> Error(e)
  // }

  use a <- result.try(setup())
  use b <- result.try(action(a))
  use c <- result.try(record(a, b))

  Ok(#(name, c))
}
