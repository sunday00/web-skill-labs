import { StrictMode } from 'react'
import { createRoot } from 'react-dom/client'
import { BrowserRouter } from 'react-router'
import { ApolloClient, HttpLink, InMemoryCache } from '@apollo/client'
import { ApolloProvider } from '@apollo/client/react'
import { Provider } from '@/components/ui/provider.tsx'
import { RemoteConfigProvider } from '@/utils/firebase/provider.remote-config.tsx'
import App from './App.tsx'
import './index.css'

const client = new ApolloClient({
  link: new HttpLink({ uri: `${import.meta.env.VITE_APP_API}/graphql` }),
  cache: new InMemoryCache(), // Apollo's caching system
})

createRoot(document.getElementById('root')!).render(
  <StrictMode>
    <BrowserRouter>
      <Provider>
        <ApolloProvider client={client}>
          <RemoteConfigProvider>
            <App />
          </RemoteConfigProvider>
        </ApolloProvider>
      </Provider>
    </BrowserRouter>
  </StrictMode>,
)
