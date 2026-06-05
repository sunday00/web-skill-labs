import { Box, Show, Stack } from '@chakra-ui/react'
import { type MouseEvent, type TouchEvent, useState } from 'react'
import Counter from '@/views/game/bana/Counter.tsx'
import { useInterval } from '@/hooks/useSetInterval.ts'

const BanaGame = () => {
  const [members, setMembers] = useState<{
    [k: string]: { x: number; y: number; opacity: number }
  }>({})

  const [counter, setCounter] = useState(5)
  const [startCounterDelay, setStartCounterDelay] = useState<number | null>(
    null,
  )

  const [gameEnd, setGameEnd] = useState<boolean>(false)

  useInterval(() => {
    setCounter(counter - 1)

    if (counter > 1 || gameEnd) return

    const pick = Math.floor(Math.random() * Object.keys(members).length)
    const key = Object.keys(members)[pick]

    const newMembers = { ...members }
    Object.keys(newMembers).forEach((k) => {
      if (k === key) newMembers[k].opacity = 1
      else newMembers[k].opacity = 0.4
    })

    setMembers(newMembers)
    setGameEnd(true)
  }, startCounterDelay)

  const startTouch = (e: TouchEvent) => {
    if (gameEnd) return

    const updatedTouches = { ...members }

    Array.from(e.changedTouches).forEach((touch) => {
      updatedTouches[touch.identifier] = {
        x: touch.clientX,
        y: touch.clientY,
        opacity: 1,
      }
    })

    setMembers(updatedTouches)

    if (Object.keys(members).length >= 1 && startCounterDelay === null)
      setStartCounterDelay(1000)
  }

  const endTouch = (e: TouchEvent) => {
    if (gameEnd) return

    const updatedTouches = { ...members }

    Array.from(e.changedTouches).forEach((touch) => {
      delete updatedTouches[touch.identifier]
    })

    setMembers(updatedTouches)

    if (Object.keys(members).length <= 1 && !gameEnd) {
      setStartCounterDelay(null)
      setCounter(5)
    }
  }

  const onRestart = (_e: MouseEvent) => {
    setCounter(5)
    setMembers({})
    setStartCounterDelay(null)
    setGameEnd(false)
  }

  return (
    <Stack>
      <Box
        as={'div'}
        w={'full'}
        h={'70vh'}
        my={4}
        borderWidth={1}
        onTouchStart={startTouch}
        onTouchMove={startTouch}
        onTouchEnd={endTouch}
        onTouchCancel={endTouch}
        style={{ touchAction: 'none' }}
      >
        {Object.entries(members).map(([id, pos], i) => (
          <div
            key={id}
            style={{
              position: 'absolute',
              left: pos.x - 40, // 중심점 조정
              top: pos.y - 40,
              // width: '80px',
              // height: '80px',
              // borderRadius: '50%',
              // backgroundColor: `hsl(${(Number(id) * 45) % 360}, 70%, 60%)`, // 손가락마다 다른 색상
              pointerEvents: 'none', // div가 터치 이벤트를 가로막지 않도록 설정
              // boxShadow: '0 4px 10px rgba(0,0,0,0.2)',
              // display: 'flex',
              // alignItems: 'center',
              // justifyContent: 'center',
              // color: 'white',
              // fontWeight: 'bold',
              fontSize: '6em',
              opacity: pos.opacity,
            }}
          >
            {['🐼', '🐯', '🐰', '🐙', '🪼', '🦊', '🐻', '🦄', '🍄‍🟫'][Number(i)]}
          </div>
        ))}

        <Show when={counter > 0 && Object.keys(members).length >= 2}>
          <Counter num={counter} />
        </Show>
      </Box>
      <Show when={gameEnd}>
        <Box onClick={onRestart}>Restart</Box>
      </Show>
    </Stack>
  )
}

export default BanaGame
