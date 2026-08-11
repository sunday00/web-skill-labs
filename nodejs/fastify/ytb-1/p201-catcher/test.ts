import { test } from 'node:test'
import { build } from './app.ts'
import assert from 'node:assert/strict'

test('test', async (t) => {
  const app = await build()

  t.after(async () => {
    await app.close()
  })

  const res = await app.inject({ method: 'GET', url: '/' })

  assert.equal(res.statusCode, 200)
  assert.equal(res.headers['content-type'], 'application/json; charset=utf-8')
  assert.equal(res.payload, JSON.stringify({ hello: 'world' }))
  assert.deepEqual(res.json(), { hello: 'world' })
  // app.close()
})
