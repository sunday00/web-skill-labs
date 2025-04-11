import gleam/int
import gleam/io
import sub.{AA}

pub fn main() {
  io.println(sub.hello())

  let aa = AA(1)
  let bb = sub.new_bb(1)

  io.println(
    int.to_string(aa.inner) <> "" <> int.to_string(sub.get_bb_inner(bb)),
  )
}
