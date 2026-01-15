import { describe, expect, it } from 'bun:test'
import app from '../index'

describe('test main app', () => {
  it('should return Hello World', async () => {
    const text = await (
      app.fetch(new Request('http://localhost:8081/pm')) as Promise<Response>
    ).then((res: Response) => res.text())

    expect(text).toBe('1')
  })
})
