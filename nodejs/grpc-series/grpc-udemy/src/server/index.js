const fs = require('fs')
const grpc = require('@grpc/grpc-js')
const greetServiceImpl = require('../greet/greet.service_impl')
const calculatorServiceImpl = require('../calculator/calculator.service_impl')
const { GreetServiceService } = require('../greet/proto/greet_grpc_pb')
const { CalculatorService } = require('../calculator/proto/calculator_grpc_pb')

const addr = '0.0.0.0:50051'

const cleanup = (server) => {
  console.log('clean up')

  if (server) server.forceShutdown()
}

const main = () => {
  const server = new grpc.Server()
  const tls = false
  let creds = grpc.ServerCredentials.createInsecure()

  if (tls) {
    const rootCert = fs.readFileSync('./ssl/ca.crt')
    const cert_chain = fs.readFileSync('./ssl/server.crt')
    const private_key = fs.readFileSync('./ssl/server.pem')

    creds = grpc.ServerCredentials.createSsl(rootCert, [
      { cert_chain, private_key },
    ])
  }

  process.on('SIGINT', () => {
    console.log('Caught interrupt signal')
    cleanup(server)
  })

  server.addService(GreetServiceService, greetServiceImpl)
  server.addService(CalculatorService, calculatorServiceImpl)

  server.bindAsync(addr, creds, (err, _) => {
    if (err) {
      return cleanup(server)
    }
  })

  console.log(`Listening on ${addr}`)
}

main()
