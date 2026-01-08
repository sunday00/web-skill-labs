import { gql } from '@apollo/client'
import { useQuery } from '@apollo/client/react'
import { Spinner } from '@chakra-ui/react'

const q = gql`
  query simpleHello {
    simpleHello
  }
`

const BackendConn = () => {
  const { loading, error, data } = useQuery<{ simpleHello: string }>(q)

  if (loading || !data || !data.simpleHello) return <Spinner />

  if (error) {
    alert(error.message)
  }

  return <>{data.simpleHello}</>
}

export default BackendConn
