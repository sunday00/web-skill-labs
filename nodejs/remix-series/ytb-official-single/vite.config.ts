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

const routesObject: { [k: string]: unknown } = {}

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
          const generateNestedRoutes = async (
            parent: { [k: string]: unknown },
            routePath: string,
            nested: string[],
          ) => {
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

                if (!Object.keys(parent).includes(routeNs)) {
                  parent[routeNs] = {
                    path: parent.path + `${pathParam !== '' ? '/' + pathParam : ''}`,
                  }
                }

                if (nested.length) {
                  return R(
                    pathParam,
                    `${curRoutePath}/route.tsx`,
                    async () =>
                      await generateNestedRoutes(
                        parent[routeNs] as { [k: string]: object },
                        curRoutePath,
                        nested,
                      ),
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
                const nested = fs
                  .readdirSync(path.join(process.cwd(), 'app', 'routes', route))
                  .filter((n) => {
                    return n !== 'route.tsx' && !n.endsWith('.ts') && n !== 'components'
                  })

                const routePath = `routes/${route}`
                routesObject[route] = { path: '/' + route }

                // @comment
                // only single page has this routes
                if (nested.length === 0) {
                  R(route, routePath + '/route.tsx')
                  return
                }

                if (!hasLayout) {
                  return () => {}
                }

                R(
                  route,
                  routePath + '/route.tsx',
                  // @comment: generate child route automatic by folder tree
                  async () =>
                    await generateNestedRoutes(
                      routesObject[route] as {
                        [k: string]: unknown
                      },
                      routePath,
                      nested,
                    ),
                )
              }),
          )

          fs.writeFileSync(
            path.join(process.cwd(), 'app', 'common', 'routes.tsx'),
            'export const routes = ' + JSON.stringify(routesObject, null, 2),
          )
        })
      },
    }),
    tsconfigPaths(),
  ],
})
