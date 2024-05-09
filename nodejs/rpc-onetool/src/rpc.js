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

const ExpensiveDbOperation = (payload, resData) => {
  console.log(payload)
  console.log(resData)

  return new Promise((res, rej) => {
    setTimeout(() => {
      res(resData)
    }, 3000)
  })
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
        // const response = { fakeResponse, payload }
        const response = await ExpensiveDbOperation(
          payload,
          fakeResponse,
        )

        channel.sendToQueue(
          msg.properties.replyTo,
          Buffer.from(JSON.stringify(response)),
          {
            correlationId: msg.properties.correlationId,
          },
        )

        channel.ack(msg)
      }
    },
    {
      noAck: false,
    },
  )
}

const requestData = async (
  RPC_QUEUE_NAME,
  payload,
  key,
) => {
  const channel = await getChannel()
  const q = await channel.assertQueue('', {
    exclusive: true,
  })

  channel.sendToQueue(
    RPC_QUEUE_NAME,
    Buffer.from(JSON.stringify(payload)),
    {
      replyTo: q.queue,
      correlationId: key,
    },
  )

  return new Promise((resolve, reject) => {
    const timeout = setTimeout(() => {
      channel.close()
      resolve('TimeOut')
    }, 8000)

    channel.consume(
      q.queue,
      (msg) => {
        if (msg.properties.correlationId === key) {
          resolve(JSON.parse(msg.content.toString()))
          clearTimeout(timeout)
        } else {
          reject('not found')
        }
      },
      { noAck: true },
    )
  })
}

const RPCRequest = async (RPC_QUEUE_NAME, payload) => {
  const k = uuid()

  return requestData(RPC_QUEUE_NAME, payload, k)
}

module.exports = {
  getChannel,
  RPCObserver,
  RPCRequest,
}
