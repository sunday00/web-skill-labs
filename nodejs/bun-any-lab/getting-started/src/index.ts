import { Elysia } from 'elysia'
import { swagger } from '@elysiajs/swagger'

import { fetchTest } from './domain/fetch-test/fetch-test.route'
import cors from '@elysiajs/cors'

const app = new Elysia()
  .use(
    cors({
      origin: 'http://localhost:3013',
    }),
  )
  .use(swagger())
  .onError(({ error, code }) => {
    if (code === 'NOT_FOUND') return 'Not Found :('

    console.error(error)
  })
  .use(fetchTest)
  .listen(3400)
