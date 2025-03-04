import * as fs from 'node:fs'
import * as path from 'node:path'

export const db = async (options: {
  where: { movieId: string }
  orderBy: { createdAt: 'asc' | 'desc' }
}) => {
  const fileData = fs.readFileSync(
    path.join(process.cwd(), 'backends', `comments.${options.where.movieId}.json`),
    'utf8',
  )

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
}
