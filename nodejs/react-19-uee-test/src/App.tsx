import { Container } from '@/components/ui/container.tsx'

function App() {
  return (
    <Container
      className="container border-2 border-blue-500"
      variant="fullMobileConstrainedBreakpointPadded"
    >
      <div className="border-2 border-red-500 h-[800px]">
        <p>SUCCESS</p>
      </div>
    </Container>
  )
}

export default App
