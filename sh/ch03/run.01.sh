node ch03/index.node.js & job1=$!
node ch03/index.node.js & job2=$!
node ch03/index.node.js & job3=$!

echo $job1 $job2 $job3

sleep 3

echo 'stop job1 & job2 >> 터미널에서 runtime 상의 process 중 ctrl + z 와 같음.'
kill -STOP $job1 # stop process then process gose background
kill -STOP $job2

jobs # list of background process

sleep 3

echo 'continue job1 & job2 >> 기존 빈 터미널 상태에서 ctrl + z 와 같음'
kill -CONT $job1 # re enter to runtime process
kill -CONT $job2
# == 터미널에서 'fg %1'
# %1, %2 는 `jobs` 명령어로 표시되는 숫자

sleep 3

echo 'kill all jobs >> 해당 pid 의 앱 종료'
kill -INT $job1
kill -INT $job2
kill -INT $job3

# job1, job2 는 중단되었다가 재개 되므로 3번과 숫자 차이가 나는 것을 확인하면 동작 확인 완료.

# 그밖에 ctrl + d : end of file
## 환경에 따라 창닫기, process 종료, 입력의 끝 알림 등...
## 솔까 모르겠다....;; ㅋ