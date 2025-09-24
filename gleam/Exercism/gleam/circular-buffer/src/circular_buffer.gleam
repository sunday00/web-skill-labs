import gleam/list

pub opaque type CircularBuffer(t) {
  CircularBuffer(data: List(t), len: Int)
}

pub fn new(capacity: Int) -> CircularBuffer(t) {
  CircularBuffer([], capacity)
}

pub fn read(buffer: CircularBuffer(t)) -> Result(#(t, CircularBuffer(t)), Nil) {
  case buffer.data |> list.first {
    Ok(f) ->
      #(f, CircularBuffer(..buffer, data: buffer.data |> list.drop(1))) |> Ok
    _ -> Error(Nil)
  }
}

pub fn write(
  buffer: CircularBuffer(t),
  item: t,
) -> Result(CircularBuffer(t), Nil) {
  case buffer.data |> list.length, buffer.len {
    dl, l if dl >= l -> Error(Nil)
    _, _ ->
      CircularBuffer(..buffer, data: buffer.data |> list.append([item])) |> Ok
  }
}

pub fn overwrite(buffer: CircularBuffer(t), item: t) -> CircularBuffer(t) {
  case buffer.data |> list.length, buffer.len {
    dl, l if dl >= l -> {
      CircularBuffer(
        ..buffer,
        data: buffer.data |> list.drop(1) |> list.append([item]),
      )
    }
    _, _ -> {
      CircularBuffer(..buffer, data: buffer.data |> list.append([item]))
    }
  }
}

pub fn clear(buffer: CircularBuffer(t)) -> CircularBuffer(t) {
  CircularBuffer(..buffer, data: [])
}
