import { Navigate, Route, Routes } from 'react-router'
import BanaGame from '@/views/game/bana'
import { List } from '@chakra-ui/react'

const Game = () => {
  return (
    <>
      <Routes>
        <Route path="/bana" element={<BanaGame />} />

        <Route path="/" element={<Navigate to="/game/bana" replace />} />
      </Routes>

      <List.Root>
        <List.Item>
          <a href="/game/bana">Bana</a>
        </List.Item>
      </List.Root>
    </>
  )
}

export default Game
