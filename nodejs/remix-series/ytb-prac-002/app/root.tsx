import { Link, Links, Meta, Outlet, Scripts, ScrollRestoration } from '@remix-run/react'
import { LinksFunction } from '@remix-run/node'

import './tailwind.css'
import { ReactNode } from 'react'

export const links: LinksFunction = () => [
  { rel: 'preconnect', href: 'https://fonts.googleapis.com' },
  {
    rel: 'preconnect',
    href: 'https://fonts.gstatic.com',
    crossOrigin: 'anonymous',
  },
  {
    rel: 'stylesheet',
    href: 'https://fonts.googleapis.com/css2?family=Inter:ital,opsz,wght@0,14..32,100..900;1,14..32,100..900&display=swap',
  },
]

export function Layout({ children }: { children: ReactNode }) {
  return (
    <html lang="en" data-theme={'cupcake'}>
      <head>
        <title>Default Title</title>
        <meta charSet="utf-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1" />
        <Meta />
        <Links />
      </head>
      <body>
        <div>
          <nav className={'ps-10 pt-5'}>
            <Link to={'/'} prefetch={'intent'} className={'text-2xl font-semibold'}>
              Home
            </Link>
          </nav>
        </div>
        {children}
        <ScrollRestoration />
        <Scripts />
      </body>
    </html>
  )
}

export default function App() {
  return <Outlet />
}
