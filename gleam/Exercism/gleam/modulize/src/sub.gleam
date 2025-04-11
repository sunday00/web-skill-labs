pub fn hello() {
  "hello"
}

pub type AA {
  AA(inner: Int)
}

pub opaque type BB {
  BB(inner: Int)
}

pub fn new_bb(inner: Int) {
  BB(inner)
}

pub fn get_bb_inner(bb: BB) {
  bb.inner
}
