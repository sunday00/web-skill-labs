import { Link } from '@remix-run/react'
import { Article } from '@/entities.extends/board.extend.entity'

export default function ArticleListItem({ entity }: { entity: Article }) {
  return (
    <li>
      <Link to={`/articles/${entity.id}`} className={'flex gap-2'}>
        <span>
          {entity.id} {entity.title} {entity.userName} {entity.createdAt}
        </span>
      </Link>
    </li>
  )
}
