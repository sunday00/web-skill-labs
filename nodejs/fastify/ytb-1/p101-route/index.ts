import fastify from 'fastify'
import scalarRef from '@scalar/fastify-api-reference'
import swagger from '@fastify/swagger'

const opts = {
  logger: {},
}
if (process.stdout.isTTY) {
  opts.logger = {
    transport: { target: 'pino-pretty' },
  }
}

// const app = fastify({ logger: true })
const app = fastify(opts)

// await app.register(import('@fastify/swagger'))
// await app.register(import('@fastify/swagger-ui'), {
//   routePrefix: '/doc',
//   uiConfig: {
//     docExpansion: 'full',
//     deepLinking: false,
//   },
//   uiHooks: {
//     onRequest: function (request, reply, next) {
//       next()
//     },
//     preHandler: function (request, reply, next) {
//       next()
//     },
//   },
//   staticCSP: true,
//   transformStaticCSP: (header) => header,
//   transformSpecification: (swaggerObject, request, reply) => {
//     return swaggerObject
//   },
//   transformSpecificationClone: true,
// })
await app.register(swagger, {
  openapi: { info: { title: 'practice', version: '1.0.0' } },
})
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
})

app.get('/', (req, res) => {
  return { hello: 'world' }
})

app.get('/k', (req, res) => {
  return { hello: 'worlds' }
})

await app.ready()

await app.listen({ port: 3120, host: '0.0.0.0' })
