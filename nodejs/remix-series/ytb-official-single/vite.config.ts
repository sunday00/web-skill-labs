import { vitePlugin as remix } from '@remix-run/dev'
import { defineConfig } from 'vite'
import tsconfigPaths from 'vite-tsconfig-paths'
import mkcert from 'vite-plugin-mkcert'
import * as fs from 'node:fs'
import path from 'node:path'

declare module '@remix-run/node' {
  interface Future {
    v3_singleFetch: true
  }
}

export default defineConfig({
  server: {
    allowedHosts: ['0.0.0.0', 'localhost', '127.0.0.1', 'ex-mac-98.local'],
    proxy: {},
  },
  plugins: [
    mkcert(),
    remix({
      future: {
        v3_fetcherPersist: true,
        v3_relativeSplatPath: true,
        v3_throwAbortReason: true,
        v3_singleFetch: true,
        v3_lazyRouteDiscovery: true,
      },
      routes(defineRoutes) {
        return defineRoutes(async (R) => {
          const generateNestedRoutes = async (routePath: string, nested: string[]) => {
            await Promise.all(
              nested.map(async (routeNs) => {
                if (routeNs.endsWith('.ts')) return () => {}
                if (routeNs === 'route.tsx') return () => {}
                if (routeNs === 'components') return () => {}

                const pathParam = routeNs.startsWith('$')
                  ? routeNs.replace('$', ':')
                  : routeNs.startsWith('_')
                    ? ''
                    : routeNs

                const curRoutePath = routePath + `/${routeNs}`
                const nested = fs
                  .readdirSync(path.join(process.cwd(), 'app', curRoutePath))
                  .filter((n) => {
                    return n !== 'route.tsx' && !n.endsWith('.ts') && n !== 'components'
                  })

                if (nested.length) {
                  return R(
                    pathParam,
                    `${curRoutePath}/route.tsx`,
                    async () => await generateNestedRoutes(curRoutePath, nested),
                  )
                }

                if (pathParam === '') {
                  return R(pathParam, `${curRoutePath}/route.tsx`, { index: true })
                }

                return R(pathParam, `${curRoutePath}/route.tsx`)
              }),
            )
          }

          const routes = fs.readdirSync(path.join(process.cwd(), 'app', 'routes'))
          await Promise.all(
            routes
              .filter((r) => !r.startsWith('_'))
              .map(async (route) => {
                const hasLayout = fs.existsSync(
                  path.join(process.cwd(), 'app', 'routes', route, 'route.tsx'),
                )
                const nested = fs.readdirSync(path.join(process.cwd(), 'app', 'routes', route))

                if (nested[0] === 'route.tsx' && nested.length === 0) {
                  return () => {}
                }

                if (!hasLayout) {
                  return () => {}
                }

                const routePath = `routes/${route}`

                R(
                  route,
                  routePath + '/route.tsx',
                  async () => await generateNestedRoutes(routePath, nested),
                )
              }),
          )

          // R('auth', 'routes/auth/route.tsx', () => {
          //   R('sns', 'routes/auth/sns/route.tsx')
          //   R('sns/:provider/callback', 'routes/auth/sns/callback.tsx')
          //   R('signup', 'routes/auth/signup/route.tsx')
          //   R('signin', 'routes/auth/signin/route.tsx')
          //   R('signout', 'routes/auth/signout/route.tsx')
          // })
          //
          // R('articles', 'routes/articles/route.tsx', () => {
          //   R('', 'routes/articles/list/route.tsx', { index: true })
          //   R(':id', 'routes/articles/id/route.tsx')
          // })
        })
      },
    }),
    tsconfigPaths(),
  ],
})
