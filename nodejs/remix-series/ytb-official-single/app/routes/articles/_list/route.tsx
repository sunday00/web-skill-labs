import { LoaderFunction } from '@remix-run/node'
import { CommonListPage } from '@/common/common.entity'
import { Article } from '@/entities.extends/board.extend.entity'
import { ReactNode } from 'react'
import { METHOD, refreshableFetch } from '@/common/refreshable.fetch'
import { useRefreshableLoad } from '@/hooks/use.refreshable'
import ArticleListItem from '@/routes/articles/_list/components/list.item'
import Flex from '@/components/layouts/flex'
import { Link } from '@remix-run/react'
import { routes } from '@/common/routes'

export const loader: LoaderFunction = async ({ request }) => {
  const url = `/board`

  return refreshableFetch({ request, method: METHOD.GET, url, data: { page: 1, size: 10 } })
}

export default function Articles() {
  const articles = useRefreshableLoad<CommonListPage<Article>>()

  const raw = 'data' in articles ? articles.data.items : []
  const list = raw.map((article: Article) => {
    return <ArticleListItem key={article.id} entity={article} />
  })

  return (
    <section>
      <ul>{list as ReactNode}</ul>

      <Flex className="mt-4 justify-end">
        <Link to={routes.articles.create.path} className={'btn'}>
          create
        </Link>
      </Flex>
    </section>
  )
}
