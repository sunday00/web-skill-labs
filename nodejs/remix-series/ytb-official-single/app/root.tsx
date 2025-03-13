import { Links, Meta, Outlet, Scripts, ScrollRestoration, useLoaderData } from '@remix-run/react'
import { LinksFunction, LoaderFunction } from '@remix-run/node'

import './tailwind.css'
import { ReactNode } from 'react'
import Navigation from '@/components/navigation'
import { Providers } from '@/providers/global.context.provider'
import '@/css/scrollbar.css'
import { getCookie, parseJwt } from '@/routes/auth/signin/cookie.manager'

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

export const loader: LoaderFunction = async ({ request }) => {
  const accessToken = await getCookie('access-token', request)
  return accessToken ? { accessToken, user: parseJwt(accessToken) } : {}
  // return {}
}

export function Layout({ children }: { children: ReactNode }) {
  const data = useLoaderData<typeof loader>()

  return (
    <html lang="en" data-theme={'winter'}>
      <head>
        <title>Default Title</title>
        <meta charSet="utf-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1" />
        <Meta />
        <Links />
      </head>
      <body className={'bg-transparent min-h-screen'}>
        <Navigation user={data?.user} />
        <main className={'container p-8 mx-auto'}>{children}</main>
        <ScrollRestoration />
        <Scripts />
      </body>
    </html>
  )
}

export default function App() {
  return (
    <Providers>
      <Outlet />
    </Providers>
  )
}
