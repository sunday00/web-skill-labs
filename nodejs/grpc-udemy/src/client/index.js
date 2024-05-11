const grpc = require('@grpc/grpc-js')
const { GreetServiceClient } = require('../greet/proto/greet_grpc_pb')
const { GreetRequest } = require('../greet/proto/greet_pb')

const doGreet = (client) => {
  console.log('doGreet was invoked')

  const req = new GreetRequest()
    .setFirstName('Clement')

  client.greet(req, (err, res) => {
    if (err) {
      return console.error(err)
    }

    console.log(`Greet: ${res.getResult()}`)
  })
}

const main = () => {
  const creds = grpc.ChannelCredentials.createInsecure()
  const client = new GreetServiceClient('127.0.0.1:50051', creds)

  doGreet(client)

  client.close()
}

main();