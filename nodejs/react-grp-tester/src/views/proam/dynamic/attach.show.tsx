import { useParams } from 'react-router'
import { gql } from '@apollo/client'
import { useQuery } from '@apollo/client/react'
import type { Feed } from '@/graphql/types.ts'
import { gqlCommonContext } from '@/utils/gql.util.ts'
import { Box, For, Spinner } from '@chakra-ui/react'

const q = gql`
  query Feed($feedId: String!) {
    FeedQuery {
      feed(id: $feedId) {
        item {
          content
          attaches {
            sortKey
            attach {
              id
              type
              path
              status
              metadata {
                sizes {
                  wh
                  name
                }
              }
            }
          }
        }
      }
    }
  }
`

const AttachShow = () => {
  const { id } = useParams()

  const { loading, error, data } = useQuery<{
    FeedQuery: { feed: { item: Feed } }
  }>(q, {
    variables: {
      feedId: id,
    },
    context: gqlCommonContext(),
  })

  if (loading || !data || !data.FeedQuery.feed.item) return <Spinner />

  if (error) {
    alert(error.message)
  }

  const feed = data.FeedQuery.feed.item
  const sorted = [...feed.attaches]
  sorted.sort((a, b) => {
    return a.sortKey - b.sortKey
  })

  console.log(sorted)

  return (
    <>
      <p>{feed.content}</p>

      <For each={sorted}>
        {(attach) => {
          return (
            <Box key={attach.attach.id}>
              <img
                src={`https://stproamtmp.blob.core.windows.net/${attach.attach.path}/${attach.attach.id}.png`}
                alt=""
              />
            </Box>
          )
        }}
      </For>
    </>
  )
}

export { AttachShow }
