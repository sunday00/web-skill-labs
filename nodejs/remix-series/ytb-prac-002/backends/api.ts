import * as fs from 'node:fs'
import * as path from 'node:path'

export const db = {
  async select(options: { where: { movieId: string }; orderBy: { createdAt: 'asc' | 'desc' } }) {
    const fp = path.join(process.cwd(), 'backends', `comments.${options.where.movieId}.json`)

    if (!fs.existsSync(fp)) {
      return []
    }

    const fileData = fs.readFileSync(fp, 'utf8')

    const j = JSON.parse(fileData)?.data

    j.sort(
      (
        a: { createdAt: string },
        b: {
          createdAt: string
        },
      ) =>
        options.orderBy.createdAt === 'asc'
          ? a.createdAt.localeCompare(b.createdAt)
          : b.createdAt.localeCompare(a.createdAt),
    )

    return j
  },
  async create(options: { movieId: number; message: string }) {
    const fp = path.join(process.cwd(), 'backends', `comments.${options.movieId}.json`)

    if (!fs.existsSync(fp)) {
      const data = {
        status: 200,
        data: [
          {
            movieId: options.movieId,
            id: 1,
            createdAt: new Date()
              .toISOString()
              .replace('T', ' ')
              .replace(/\.\d{3}Z/, ''),
            message: options.message,
          },
        ],
      }

      fs.writeFileSync(fp, JSON.stringify(data, null, 2))
      return
    }

    const raw: {
      stats: 200
      data: { movieId: number; id: number; createdAt: string; message: string }[]
    } = JSON.parse(fs.readFileSync(fp, 'utf8'))

    const lastId = raw.data.sort((a, b) => a.id - b.id)[raw.data.length - 1]?.id

    const newComment = {
      movieId: options.movieId,
      id: lastId + 1,
      createdAt: new Date()
        .toISOString()
        .replace('T', ' ')
        .replace(/\.\d{3}Z/, ''),
      message: options.message,
    }

    raw.data.push(newComment)

    fs.writeFileSync(fp, JSON.stringify(raw, null, 2))
  },
}
