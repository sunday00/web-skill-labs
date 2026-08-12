import 'fastify'

// define types for decorator

declare module 'fastify' {
  interface FastifyRequest {
    user: string | undefined
  }

  interface FastifyInstance {
    global: any
  }
}
