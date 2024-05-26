import * as grpc from '@grpc/grpc-js'
import * as protoLoader from '@grpc/proto-loader'
import { ProtoGrpcType } from './domain/hero'
import { heroService } from './domain/hero/hero.service.impl'
const packageDefinition = protoLoader.loadSync(
  './dist/resources/grpc/domain/hero/hero.proto',
)
const proto = grpc.loadPackageDefinition(
  packageDefinition,
) as unknown as ProtoGrpcType

const addr = '0.0.0.0:50052'

const cleanup = async (server: grpc.Server) => {
  console.log('clean up')
  server.forceShutdown()
}

const main = async () => {
  const server = new grpc.Server()
  let creds = grpc.ServerCredentials.createInsecure()

  process.on('SIGINT', () => {
    console.log('Caught interrupt signal')
    cleanup(server)
  })

  server.addService(proto.hero.HeroesService.service, heroService)

  server.bindAsync(addr, creds, (err, _) => {
    if (err) {
      console.log(err)
      return cleanup(server)
    }
  })

  console.log(`Listening on ${addr}`)

  return server
}

main().catch(cleanup)
