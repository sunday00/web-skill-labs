import { Elysia } from 'elysia'
import { swagger } from '@elysiajs/swagger'

import { fetchTest } from './domain/fetch-test/fetch-test.route'
import cors from '@elysiajs/cors'
import { todayShould } from './domain/today-should/today-should.route'

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
  .use(todayShould)
  .listen(3400)
