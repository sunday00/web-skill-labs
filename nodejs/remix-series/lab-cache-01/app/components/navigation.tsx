import { Link } from '@remix-run/react'

export default function Navigation() {
  return (
    <nav className="navbar bg-base-100 shadow-sm">
      <div className="navbar-start">
        <a href={'/'} className="btn btn-ghost text-xl">
          daisyUI
        </a>
      </div>

      <div className="navbar-center hidden lg:flex">
        <ul className="menu menu-horizontal px-1">
          <li>
            <Link to={'/articles'}>게시판</Link>
          </li>
        </ul>
      </div>
    </nav>
  )
}
