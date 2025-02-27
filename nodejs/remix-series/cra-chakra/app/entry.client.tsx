import { RemixBrowser } from '@remix-run/react'
import { startTransition, StrictMode } from 'react'
import { hydrateRoot } from 'react-dom/client' // import { ChakraProvider } from './components/chakra-provider'
import { CacheProvider } from '@emotion/react'
import createCache from '@emotion/cache'

const hydrate = () => {
  const cache = createCache({ key: 'css' })

  startTransition(() => {
    hydrateRoot(
      document,
      <StrictMode>
        <CacheProvider value={cache}>
          <RemixBrowser />
        </CacheProvider>
      </StrictMode>,
    )
  })
}

if (typeof requestIdleCallback === 'function') {
  requestIdleCallback(hydrate)
} else {
  // Safari doesn't support requestIdleCallback
  // https://caniuse.com/requestidlecallback
  setTimeout(hydrate, 1)
}
