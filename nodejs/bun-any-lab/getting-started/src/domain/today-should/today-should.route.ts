import { Elysia } from 'elysia'

const todayShould = new Elysia({ prefix: '/todayShould' })
  // .decorate('user', new User())
  .get('/', async ({ set }) => {
    set.headers['content-type'] = 'text/html'
    return '<h1 style="font-size: 15em;">buy lotto</h1>'
  })

export { todayShould }
