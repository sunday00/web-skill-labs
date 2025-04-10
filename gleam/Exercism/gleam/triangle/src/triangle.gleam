fn is_triangle(a: Float, b: Float, c: Float) {
  a +. b >. c && b +. c >. a && c +. a >. b
}

pub fn equilateral(a: Float, b: Float, c: Float) -> Bool {
  a == b && b == c && c == a && is_triangle(a, b, c)
}

pub fn isosceles(a: Float, b: Float, c: Float) -> Bool {
  { a == b || b == c || c == a } && is_triangle(a, b, c)
}

pub fn scalene(a: Float, b: Float, c: Float) -> Bool {
  { a != b && b != c && c != a } && is_triangle(a, b, c)
}
