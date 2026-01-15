import { describe, expect, it } from 'bun:test'
import { treaty } from '@elysiajs/eden'
import app, { type App } from '../index'

const eden = treaty<App>(app)

describe('test main app', () => {
  it('should return Hello World', async () => {
    const text = await (
      app.fetch(new Request('http://localhost:8081/pm')) as Promise<Response>
    ).then((res: Response) => res.text())

    expect(text).toBe('1')
  })

  it('should return 1 with eden', async () => {
    const { data, error } = await eden.pm.get()

    expect(data).toBe(1) // more type safety with EDEN
  })
})
