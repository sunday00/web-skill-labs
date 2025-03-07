import { Link } from '@remix-run/react'
import { ReactNode } from 'react'

const dirs = ['articles', 'newsletters']

export default function Navigation() {
  const menus = dirs.map((d) => {
    return (
      <li key={d}>
        <Link to={`/${d}`}>{d}</Link>
      </li>
    )
  })

  return (
    <nav className="navbar bg-base-100 shadow-sm">
      <div className="navbar-start">
        <a href={'/'} className="btn btn-ghost text-xl">
          daisyUI
        </a>
      </div>

      <div className="navbar-center hidden lg:flex">
        <ul className="menu menu-horizontal px-1">{menus as ReactNode}</ul>
      </div>
    </nav>
  )
}
