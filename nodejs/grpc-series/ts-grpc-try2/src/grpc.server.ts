import * as grpc from '@grpc/grpc-js'
import { heroProto, heroService } from './domain/hero/hero.service.impl'
import { heroProto2, heroService2 } from './domain/hero2/hero.service.impl'

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

  server.addService(heroProto.hero.HeroesService.service, heroService)
  server.addService(heroProto2.hero2.HeroesService.service, heroService2)

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
