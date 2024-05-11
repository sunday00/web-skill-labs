const grpc = require('@grpc/grpc-js')
const { GreetServiceClient } = require('../greet/proto/greet_grpc_pb')
const { CalculatorClient } = require('../calculator/proto/calculator_grpc_pb')
const { GreetRequest } = require('../greet/proto/greet_pb')
const { SumRequest } = require('../calculator/proto/sum_pb')

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

const doSum = (client) => {
  console.log('doCalculator was invoked')

  const req = new SumRequest()
    .setFirstNumber(1)
    .setSecondNumber(2)

  client.sum(req, (err, res) => {
    if (err) {
      return console.error(err)
    }

    console.log(`Sum: ${res.getResult()}`)
  })
}

const main = () => {
  const creds = grpc.ChannelCredentials.createInsecure()
  const client = new GreetServiceClient('127.0.0.1:50051', creds)
  const client2 = new CalculatorClient('127.0.0.1:50051', creds)

  doGreet(client)
  doSum(client2)

  client.close()
  client2.close()
}

main();