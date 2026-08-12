import { type FastifyPluginAsync } from 'fastify'

const movie: FastifyPluginAsync = async (app, opts) => {
  app.get<{ Params: { id: number } }>(
    '/',
    {
      schema: {
        params: {
          type: 'object',
          properties: { id: { type: 'number' } },
          required: ['id'],
        },
      },
    },
    async (req, res) => {
      return { id: req.params.id }
    },
  )
}

export default movie
