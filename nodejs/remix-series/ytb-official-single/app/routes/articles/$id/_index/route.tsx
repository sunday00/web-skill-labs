import Box from '@/components/layouts/box'
import { LoaderFunction } from '@remix-run/node'
import { METHOD, refreshableFetch } from '@/common/refreshable.fetch'
import { useRefreshableLoad } from '@/hooks/use.refreshable'
import { Article } from '@/entities.extends/board.extend.entity'
import Title from '@/components/texts/title'
import { time } from '@/utils/time'

export const loader: LoaderFunction = async ({ request, params }) => {
  const url = `/board/${params.id}`
  return refreshableFetch({ request, method: METHOD.GET, url, data: {} })
}

export default function ArticleDetail() {
  const loaded = useRefreshableLoad<Article>()
  const article = 'data' in loaded ? loaded.data : null

  return (
    <section>
      <Box>
        <Title as={2} text={article?.title ?? ''} />

        <div>{article?.content}</div>

        <div>{time(article?.createdAt).fromNow()}</div>
      </Box>
    </section>
  )
}
