import { gql } from '@apollo/client'
import { useQuery } from '@apollo/client/react'
import type { Feed } from '@/graphql/types.ts'
import { gqlCommonContext } from '@/utils/gql.util.ts'
import { Box, For, Spinner, Stack } from '@chakra-ui/react'
import { Navigate, Route, Routes } from 'react-router'
import { AttachCreate } from '@/views/proam/attach/attach.create.tsx'
import { AttachShow } from '@/views/proam/attach/attach.show.tsx'

const q = gql`
  query Feeds($clubId: String!, $input: CursorGraphReq) {
    FeedQuery {
      feeds(clubId: $clubId, input: $input) {
        success
        items {
          id
        }
        hasNext
        nextCursor
        message
      }
    }
  }
`

const ProamAttachList = () => {
  let feeds: { id: string }[] = []

  const { loading, error, data } = useQuery<{
    FeedQuery: { feeds: { items: Feed[] } }
  }>(q, {
    variables: {
      clubId: '6a0fb2edf062b28e7e1a15e2',
      input: {
        search: {},
      },
    },
    context: gqlCommonContext(),
  })

  if (loading || !data || !data.FeedQuery.feeds.items) return <Spinner />

  if (error) {
    alert(error.message)
  }

  feeds = data.FeedQuery.feeds.items

  return (
    <Stack>
      <For each={feeds}>
        {(feed) => {
          return (
            <Box key={feed.id}>
              <a href={`/proam/attach/${feed.id}`}>{feed.id}</a>
            </Box>
          )
        }}
      </For>

      <a href="/proam/attach/create">create</a>
    </Stack>
  )
}
const ProamAttach = () => {
  return (
    <Routes>
      <Route path="/create" element={<AttachCreate />} />
      <Route path="/:id" element={<AttachShow />} />
      <Route path="/" element={<ProamAttachList />} />

      <Route path="/attach" element={<Navigate to="/proam/attach" replace />} />
    </Routes>
  )
}

export { ProamAttach }
