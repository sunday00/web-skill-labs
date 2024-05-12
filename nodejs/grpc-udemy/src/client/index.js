const grpc = require('@grpc/grpc-js')
const { GreetServiceClient } = require('../greet/proto/greet_grpc_pb')
const { CalculatorClient } = require('../calculator/proto/calculator_grpc_pb')
const { GreetRequest } = require('../greet/proto/greet_pb')
const { SimpleRequest } = require('../calculator/proto/simple_pb')
const { SumRequest } = require('../calculator/proto/sum_pb')
const { PrimeRequest } = require('../calculator/proto/prime_pb')

const doGreet = (client) => {
  console.log('doGreet was invoked')

  const req = new GreetRequest().setFirstName('Clement')

  client.greet(req, (err, res) => {
    if (err) {
      return console.error(err)
    }

    console.log(`Greet: ${res.getResult()}`)
  })
}

const doGreetMany = (client) => {
  console.log('doGreetMany was invoked')

  const req = new GreetRequest().setFirstName('Clement')

  const call = client.greetMany(req)

  call.on('data', (res) => {
    console.log(`GreetMany: ${res.getResult()}`)
  })
}

const doStreamSimple = (client) => {
  const req = new SimpleRequest().setNumber(1)

  const call = client.simple(req)

  call.on('data', (res) => {
    console.log(`simple: ${res.getResult()}`)
  })
}

const doSum = (client) => {
  console.log('doCalculator was invoked')

  const req = new SumRequest().setFirstNumber(1).setSecondNumber(2)

  client.sum(req, (err, res) => {
    if (err) {
      return console.error(err)
    }

    console.log(`Sum: ${res.getResult()}`)
  })
}

const doPrime = (client) => {
  console.log('doPrime was invoked')

  const req = new PrimeRequest().setNumber(84332)

  const call = client.prime(req)

  call.on('data', (res) => {
    console.log(`Prime num: ${res.getResult()}`)
  })
}

const main = () => {
  const creds = grpc.ChannelCredentials.createInsecure()
  const client = new GreetServiceClient('127.0.0.1:50051', creds)
  const client2 = new CalculatorClient('127.0.0.1:50051', creds)

  doStreamSimple(client2)

  doGreet(client)
  doGreetMany(client)

  doSum(client2)
  doPrime(client2)

  client.close()
  client2.close()
}

main()
