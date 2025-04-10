import gleam/list
import gleam/result
import gleam/string
import simplifile

pub fn read_emails(path: String) -> Result(List(String), Nil) {
  // case simplifile.read(path) {
  //   Ok(content) -> {
  //     Ok(content |> string.trim |> string.split("\n"))
  //   }
  //   Error(_) -> Error(Nil)
  // }

  simplifile.read(path)
  |> result.map(string.trim)
  |> result.map(string.split(_, "\n"))
  |> result.nil_error
}

pub fn create_log_file(path: String) -> Result(Nil, Nil) {
  // case simplifile.create_file(path) {
  //   Ok(_) -> Ok(Nil)
  //   Error(_) -> Error(Nil)
  // }

  simplifile.create_file(path) |> result.nil_error
}

pub fn log_sent_email(path: String, email: String) -> Result(Nil, Nil) {
  // case simplifile.append(path, email <> "\n") {
  //   Ok(_) -> Ok(Nil)
  //   Error(_) -> Error(Nil)
  // }

  simplifile.append(path, email <> "\n") |> result.nil_error
}

pub fn send_newsletter(
  emails_path: String,
  log_path: String,
  send_email: fn(String) -> Result(Nil, Nil),
) -> Result(Nil, Nil) {
  let _ = create_log_file(log_path)

  read_emails(emails_path)
  |> result.unwrap([])
  |> list.each(fn(email) {
    // case send_email(email) {
    //   Ok(_) -> {
    //     log_sent_email(log_path, email)
    //   }
    //   Error(_) -> Error(Nil)
    // }

    send_email(email) |> result.try(fn(_) { log_sent_email(log_path, email) })
  })

  Ok(Nil)
}
