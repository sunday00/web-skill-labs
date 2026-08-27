const arr1 = [73, 42, 18, 91, 56, 34, 87, 25, 69, 10]

const r1 = arr1.map(x => {
  console.log('call map')
  console.log(x * 2)
  return x * 2
}).filter(x => {
  console.log('call filter')
  return x > 50
}).map(x => {
  console.log('call mutation')
  return x + ''
})

console.log(r1)
console.log('\n\n=====================================\n\n')

const r2 = arr1.values().map(x => {
  console.log('call map')
  return x * 2
}).filter(x => {
  console.log('call filter')
  return x > 50
}).map(x => {
  console.log('call mutation')
  return x + ''
}).toArray()

console.log(r2)

// 정리
/**
 * 1. array 에 .values() 하는 걸로 iterator 생성 완료.
 * - 단순 배열에서 출발시 굳이 new Iterator from 뭐 어쩌고 문법을 쓸 필요는 없다.
 *
 * 2. Iterator 를 사용하는 이유는 next() 같은 중단/계속 같은 멈춤기능도 있겠지만,
 *
 * 3. 제일 편리하게 와 닿는 기능은 Lazy chanining 이다.
 * - iterator 는 toArray(), iterator의 values(), entries() 등 트리거 발동 전에는 아무 체이닝도 발생하지 않는다.
 * - 트리거가 동작하면 한 체이닝을 모든 원소가 돌지 않고, 원소별로 하나의 원소가 모든 체이닝을 개별로 돌아 모인다.
 *
 * - 동작 시뮬레이션
 *     - r1
 *         + map * 10     - 0번째 원소를 * 2, 1번째 원소를 * 2, ... 10번
 *         + filter * 10  - 0번째 원소가 50 넘는지 확인, 1번째 원소가 50 넘는지 확인, ... 10번
 *         + map * 7      - 0번째 원소를 문자열로, 1번째 원소를 문자열로, ... 7번
 *         = 27
 *     - r2
 *         + map, filter, map * 10 - 0번째 원소를 * 2해서 50넘는지 확인하고 문자열로, 1번째 원소를 * 2해서 50 안넘으면 버리고, ... 10번 끝
 *         = 10
 *
 */

