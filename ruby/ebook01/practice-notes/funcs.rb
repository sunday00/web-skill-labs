def f(n)
  n + 1
end

p f(1)

def g(a, *args)
  puts a
  puts args[0]
end

g(1, 2, 3)

def h(a, bbb)
  b1, b2 = bbb[:one], bbb[:two]
  p "#{a} #{b1} #{b2}"
end

h(1, { one: 2, two: 3 })

h 1, :one => 2, :two => 3

def sum (a, b)
  yield a + b
  # a + b      # <-- not work
end

sum(1, 2) { |n| p n }

def sum2 (a, b, &c)
  c.call(a + b)
end

sum2(1, 2) { |n| p n }

File.open('./sample.txt') {
  |f| f.each_line { |l| p l }
}

File.open('./sample.txt') do |f|
  f.each_line { |l| p l }
end

File.open('./sample.txt') do |f|
  f.each_line do
  |l|
    p l
  end
end