import { CommonListPage } from '@/common/common.entity'
import { Link } from '@remix-run/react'
import { ReactNode } from 'react'
import { FaAngleLeft, FaAngleRight, FaAnglesLeft, FaAnglesRight } from 'react-icons/fa6'

const LinkButton = ({
  children,
  i,
  isActive,
}: {
  children?: ReactNode
  i: number
  isActive?: boolean
}) => {
  const qs = new URLSearchParams(location.search)
  qs.set('page', i.toString())

  return (
    <Link
      to={`?${qs.toString()}`}
      key={i}
      className={`join-item btn ${isActive ? 'btn-active' : ''}`}
    >
      {children ?? i}
    </Link>
  )
}

export default function Paginate({ bulk }: { bulk: CommonListPage<unknown> }) {
  const totalPage = Math.ceil(bulk.total / bulk.size)
  const tenLine = Math.floor((bulk.page - 1) / 10)

  const min = tenLine * 10 + 1 >= totalPage ? Math.floor(totalPage / 10) * 10 + 1 : tenLine * 10 + 1
  const max = tenLine * 10 + 10 >= totalPage ? totalPage : tenLine * 10 + 10

  const goPrevBulkNumber = bulk.page - 10 >= 1 ? bulk.page - 10 : 1

  const numberBtns = [
    <LinkButton key={'paginate-prev-first'} i={1}>
      <FaAnglesLeft />
    </LinkButton>,
    <LinkButton key={'paginate-prev-bulk'} i={goPrevBulkNumber}>
      <FaAngleLeft />
    </LinkButton>,
  ]

  for (let i = min; i <= max; i++) {
    numberBtns.push(<LinkButton i={i} key={i} isActive={bulk.page === i} />)
  }

  const goNextBulkNumber = bulk.page + 10 >= totalPage ? totalPage : bulk.page + 10
  numberBtns.push(
    <LinkButton key={'paginate-next-bulk'} i={goNextBulkNumber}>
      <FaAngleRight />
    </LinkButton>,
  )
  numberBtns.push(
    <LinkButton key={'paginate-next-last'} i={totalPage}>
      <FaAnglesRight />
    </LinkButton>,
  )

  return <div className="join flex justify-center my-4">{numberBtns as ReactNode}</div>
}
