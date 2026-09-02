l = [1, 2, 3, 4, 5]

l.each { |el| p el if el % 2 == 0 }

for el in l
  if el % 2;
    p el
  end
end

$s = 0 # $means global
def f(i)
  if $s == 0;
    $s = 1
    0
  else
    i
  end
end

for i in (1..5)
  p "echo #{i}"

  if f(i) != 0;
    p "not 0 + #{i}"
  else
    redo # 같은 i 로 한번 더 반복. 재시도를 의미.
  end
end

# next = continue; break = break; retry = reloop
