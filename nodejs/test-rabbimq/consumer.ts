import * as amqplib from 'amqplib'

const getRabbitMqConnection = async () => {
  return await amqplib.connect(
    {
      hostname: '127.0.0.1',
      port: 5672,
      username: 'sunday00',
      password: 'example',
    },
    amqplib.credentials.external(),
  )
}

const boot = async () => {
  const conn = await getRabbitMqConnection()

  const ch = await conn.createChannel()
  await ch.assertExchange('test.nodejs.delay', 'direct', { durable: true })
  await ch.assertQueue('say.status', { durable: true })

  await ch.bindQueue('say.status', 'test.nodejs.delay', 'task.fire')

  await ch.consume('say.status', (payload) => {
    const raw = payload.content.toString('utf-8')
    const json = JSON.parse(raw)

    console.log(json)
  })
}

boot()
