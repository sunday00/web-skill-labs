const amqp = require('amqplib')
const { v4: uuid } = require('uuid')

let amqpConn = null

const getChannel = async () => {
  if (!amqpConn) {
    amqpConn = await amqp.connect('amqp://127.0.0.1:5672', {
      credentials: amqp.credentials.plain(
        'admin',
        'qwer1234',
      ),
    })
  }

  return await amqpConn.createChannel()
}

const RPCObserver = async (
  RPC_QUEUE_NAME,
  fakeResponse,
) => {
  const channel = await getChannel()
  await channel.assertQueue(RPC_QUEUE_NAME, {
    durable: false,
  })

  channel.prefetch(1)
  channel.consume(
    RPC_QUEUE_NAME,
    async (msg) => {
      if (msg.content) {
        const payload = JSON.parse(msg.content.toString())
        const response = { fakeResponse, payload }

        channel.sendToQueue(
          msg.properties.replyTo,
          Buffer.from(JSON.stringify(response)),
          {
            correlationId: msg.properties.correlationId,
          },
        )
      }
    },
    {
      noAck: false,
    },
  )
}

const RPCRequest = async () => {}
