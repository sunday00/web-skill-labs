import { Box, Span } from '@chakra-ui/react'

const Counter = ({ num }: { num: number }) => {
  return (
    <Box>
      <Span
        style={{
          fontSize: '4em',
          position: 'absolute',
          top: '6em',
          left: '50%',
          transform: 'translate(-50%, 0)',
          fontFamily: 'GoBold',
        }}
      >
        {num}
      </Span>
    </Box>
  )
}

export default Counter
