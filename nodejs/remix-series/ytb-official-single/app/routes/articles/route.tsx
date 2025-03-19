import { Outlet } from '@remix-run/react'

export default function ArticlesLayout() {
  return (
    <article className={''}>
      <h1>ARTICLES</h1>

      <Outlet />
    </article>
  )
}
