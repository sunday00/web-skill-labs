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
import Paginate from '@/components/func/page'
import { CommonUncaughtMessageShow } from '@/common/excetions'

export const loader: LoaderFunction = async ({ request }) => {
  const url = `/board`
  const qs = new URL(request.url).searchParams

  return refreshableFetch({
    request,
    method: METHOD.GET,
    url,
    data: { page: Number(qs.get('page') ?? 1), size: Number(qs.get('size') ?? 20) },
  })
}

export default function Articles() {
  const raw = useRefreshableLoad<CommonListPage<Article>>()

  if (!('data' in raw)) {
    return <CommonUncaughtMessageShow error={raw} />
  }

  const articles = raw.data
  const list = articles.items.map((article: Article) => {
    return <ArticleListItem key={article.id} entity={article} />
  })

  return (
    <section>
      <ul>{list as ReactNode}</ul>

      <Paginate bulk={articles} />

      <Flex className="mt-4 justify-end">
        <Link to={routes.articles.create.path} className={'btn btn-secondary'}>
          create
        </Link>
      </Flex>
    </section>
  )
}
