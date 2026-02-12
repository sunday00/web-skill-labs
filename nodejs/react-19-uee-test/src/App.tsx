import { Container } from '@/components/ui/container.tsx'
import { Navigate, Route, Routes } from 'react-router'
import StandardLayout from '@/layout/standard.layout.tsx'

function App() {
  return (
    <Container
      className="container"
      variant="fullMobileConstrainedBreakpointPadded"
    >
      <main className="main p-2">
        <Routes>
          <Route path="/*" element={<StandardLayout />} />

          <Route path="/" element={<Navigate to="/home" replace />} />
        </Routes>
      </main>
    </Container>
  )
}

export default App
