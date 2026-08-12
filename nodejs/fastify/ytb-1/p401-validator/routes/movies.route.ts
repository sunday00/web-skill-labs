import { type FastifyPluginAsync } from 'fastify'

const movies: FastifyPluginAsync = async (app, opts) => {
  app.post(
    '/',
    {
      schema: {
        tags: ['movie'],
        body: {
          type: 'object',
          properties: {
            title: { type: 'string', minLength: 3 },
            year: { type: 'number' },
          },
          required: ['title', 'year'],
        },
      },
    },
    async (req, res) => {
      return req.body
    },
  )

  app.post(
    '/stars',
    {
      schema: {
        tags: ['movie'],
        body: {
          type: 'object',
          properties: {
            title: { type: 'string' },
            year: { type: 'number' },
          },
          required: ['title', 'year'],
        },
      },
    },
    async (req, res) => {
      return req.body
    },
  )
}

export default movies
