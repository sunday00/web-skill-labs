import { vitePlugin as remix } from '@remix-run/dev'
import { defineConfig } from 'vite'
import tsconfigPaths from 'vite-tsconfig-paths'
import mkcert from 'vite-plugin-mkcert'

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
        return defineRoutes((R) => {
          R('auth', 'routes/auth/route.tsx', () => {
            R('sns', 'routes/auth/sns/route.tsx')
            R('sns/:provider/callback', 'routes/auth/sns/callback.tsx')
            R('signup', 'routes/auth/signup/route.tsx')
            R('signin', 'routes/auth/signin/route.tsx')
            R('signout', 'routes/auth/signout/route.tsx')
          })

          R('articles', 'routes/articles/route.tsx', () => {
            R('', 'routes/articles/list/route.tsx', { index: true })
            R(':id', 'routes/articles/id/route.tsx')
          })
        })
      },
    }),
    tsconfigPaths(),
  ],
})
