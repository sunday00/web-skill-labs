import scalarRef from '@scalar/fastify-api-reference'
import { build } from './app.ts'
import closeWithGrace from 'close-with-grace'
import dotenv from 'dotenv'
import path from 'node:path'

dotenv.config({ path: path.join(process.cwd(), 'p201-catcher', '.env') })

const opts = {
  logger: {},
}
if (process.stdout.isTTY) {
  opts.logger = {
    transport: { target: 'pino-pretty' },
  }
}

const app = await build(opts)

await app.register(scalarRef, {
  routePrefix: '/doc',
  hooks: {
    onRequest: function (request, reply, done) {
      done()
    },
    preHandler: function (request, reply, done) {
      done()
    },
  },
  configuration: {
    theme: 'default',
  },
})

await app.ready()

await app.listen({ port: Number(process.env.PORT || 3000), host: '0.0.0.0' })

closeWithGrace(async ({ err, signal }) => {
  if (err) {
    app.log.error({ err }, 'ooooops!!')
  } else {
    app.log.info({ signal }, 'bye bye')
  }

  await app.close()
})
