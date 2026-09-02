def gs
  stack = []
  push = lambda { |n| stack << n }
  pop = lambda { stack.pop }

  [push, pop, stack]
end

push1, pop1, stack1 = gs

p push1, pop1

push1.call(1)
push1.call(2)
push1.call(3)

p stack1

pop1.call

p stack1