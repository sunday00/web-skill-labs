import { Container, Heading, Spinner } from '@chakra-ui/react'
import { useEffect, useState } from 'react'

const Fallback = () => {
  const [state, setState] = useState('loading')

  useEffect(() => {
    const st = setTimeout(() => {
      setState('failed')
    }, 3000)

    return () => clearTimeout(st)
  })

  return (
    <Container>
      {state === 'loading' ? (
        <Spinner />
      ) : (
        <>
          <Heading as={'h1'}>sorry ...</Heading>

          <a href="/">Home</a>
        </>
      )}
    </Container>
  )
}

export default Fallback
