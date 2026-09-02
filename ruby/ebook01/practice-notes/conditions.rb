a = 2

case a
  when 1; p 'one'
  when 2; p 'two'
  else
    p 'three'
end

case
  when a == 1; p 'one'
  when a == 2; p 'two'
  else
    p 'three'
end

b = case
      when a == 1; 'one'
      when a == 2; 'two'
      else
        'three'
    end

p b

p 'ok' if a == 2
p 'fuck' if a != 1
p 'not one' unless a == 1

p 'h' == 'h'