import { Link } from '@remix-run/react'
import { Article } from '@/entities.extends/board.extend.entity'
import { routes } from '@/common/routes'

export default function ArticleListItem({ entity }: { entity: Article }) {
  const linkTo = routes.articles.$id.path.replace(':id', entity.id.toString())

  return (
    <li>
      <Link to={linkTo} className={'flex gap-2'}>
        <span>
          {entity.id} {entity.title} {entity.userName} {entity.createdAt}
        </span>
      </Link>
    </li>
  )
}
