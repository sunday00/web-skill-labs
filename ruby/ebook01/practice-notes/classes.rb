class Incrementor
  def initialize(val)
    @val = val
  end

  public def next
    @val += 1
  end
end

i = Incrementor.new(1)

p i.next
p i.next

class Incrementor
  def initialize(val)
    @over_init_val = val
    @val = val
  end

  attr_reader :over_init_val
end

j = Incrementor.new(1)
p j.next
p j.over_init_val

