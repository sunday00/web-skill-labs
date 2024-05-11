const grpc = require('@grpc/grpc-js')
const serviceImpl =  require('./service_impl')
const { GreetServiceService } =require('../greet/proto/greet_grpc_pb')
const { CalculatorService } =require('../calculator/proto/calculator_grpc_pb')

const addr = 'localhost:50051'

const cleanup = (server) => {
  console.log('clean up')

  if(server) server.forceShutdown()
}

const main = () => {
  const server = new grpc.Server()
  const creds = grpc.ServerCredentials.createInsecure()

  process.on('SIGINT', () => {
    console.log('Caught interrupt signal')
    cleanup(server)
  })

  server.addService(GreetServiceService, serviceImpl)
  server.addService(CalculatorService, serviceImpl)

  server.bindAsync(addr, creds, (err, _) => {
    if (err) {
      return cleanup(server)
    }
  })

  console.log(`Listening on ${addr}`)
}

main()