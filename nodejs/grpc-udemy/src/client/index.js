const fs = require('fs')
const grpc = require('@grpc/grpc-js')
const { GreetServiceClient } = require('../greet/proto/greet_grpc_pb')
const { CalculatorClient } = require('../calculator/proto/calculator_grpc_pb')
const { GreetRequest } = require('../greet/proto/greet_pb')
const { SimpleRequest } = require('../calculator/proto/simple_pb')
const { SumRequest } = require('../calculator/proto/sum_pb')
const { PrimeRequest } = require('../calculator/proto/prime_pb')
const { AvgRequest } = require('../calculator/proto/avg_pb')
const { MaxRequest } = require('../calculator/proto/max_pb')
const { SqrtRequest } = require('../calculator/proto/sqrt_pb')

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

const doLongGreet = (client) => {
  console.log('doLongGreet was invoked')

  const names = ['Clement', 'Maria', 'Tom']

  const call = client.longGreet((err, res) => {
    if (err) return console.error(err)

    console.log(`LongGreet: ${res.getResult()}`)
  })

  names
    .map((name) => {
      return new GreetRequest().setFirstName(name)
    })
    .forEach((req) => {
      call.write(req)
    })
  call.end()
}

const doGreetEveryone = (client) => {
  console.log('doGreetEveryone was invoked')

  const names = ['Clement', 'Maria', 'Tom']

  const call = client.greetEveryone()

  call.on('data', (res, err) => {
    if (err) return console.error(err)

    console.log(`GreetEveryone: ${res.getResult()}`)
  })

  names
    .map((name) => {
      return new GreetRequest().setFirstName(name)
    })
    .forEach((req) => {
      call.write(req)
    })
  call.end()
}

const doGreetWithDeadLine = (client, ms) => {
  console.log('doGreetWithDeadLine was invoked')

  const req = new GreetRequest().setFirstName('Clement')

  client.greetWithDeadline(
    req,
    { deadline: new Date(Date.now() + ms) },
    (err, res) => {
      if (err) return console.error(err)

      console.log(`GreetWithDeadline: ${res.getResult()}`)
    },
  )
}

const doStreamSimple = (client) => {
  const req = new SimpleRequest().setNumber(1)

  const call = client.simple(req)

  call.on('data', (res) => {
    console.log(`simple: ${res.getResult()}`)
  })
}

const doSum = (client) => {
  console.log('doSum was invoked')

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

const doAvg = (client) => {
  console.log('doAvg was invoked')

  const numbers = [...Array(11).keys()].slice(1)

  const call = client.avg((err, res) => {
    if (err) {
      return console.error(err)
    }

    console.log(`Avg: ${res.getResult()}`)
  })

  numbers
    .map((number) => {
      return new AvgRequest().setNumber(number)
    })
    .forEach((req) => {
      call.write(req)
    })

  call.end()
}

const doMax = (client) => {
  console.log('doMax was invoked')

  const numbers = [4, 7, 2, 19, 4, 6, 32]
  const call = client.max()

  numbers.forEach((number) => {
    const req = new MaxRequest().setNumber(number)
    call.write(req)
  })

  call.on('data', (res) => {
    console.log(`Max num: ${res.getResult()}`)
  })

  call.end()
}

const doSqrt = (client, n) => {
  console.log('doSqrt was invoked')

  const req = new SqrtRequest().setNumber(n)

  client.sqrt(req, (err, res) => {
    if (err) {
      return console.error(err)
    }

    console.log(`sqrt: ${res.getResult()}`)
  })
}

const main = () => {
  let creds = grpc.ChannelCredentials.createInsecure()

  const tls = false
  if (tls) {
    const rootCert = fs.readFileSync('./ssl/ca.crt')

    creds = grpc.ChannelCredentials.createSsl(rootCert)
  }

  const client = new GreetServiceClient('127.0.0.1:50051', creds)
  const client2 = new CalculatorClient('127.0.0.1:50051', creds)

  // doStreamSimple(client2)
  //
  doGreet(client)
  // doGreetMany(client)
  // doLongGreet(client)
  // doGreetEveryone(client)
  // doGreetWithDeadLine(client, 5000)
  // doGreetWithDeadLine(client, 1000)
  //
  // doSum(client2)
  // doPrime(client2)
  // doAvg(client2)
  // doMax(client2)
  // doSqrt(client2, -1)
  // doSqrt(client2, 9)

  client.close()
  client2.close()
}

main()
