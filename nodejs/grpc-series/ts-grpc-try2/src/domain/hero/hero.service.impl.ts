import { HeroesServiceHandlers } from './HeroesService'
import {
  sendUnaryData,
  ServerDuplexStream,
  ServerUnaryCall,
} from '@grpc/grpc-js'
import { HeroById__Output } from './HeroById'
import { Hero } from './Hero'

export const heroService: HeroesServiceHandlers = {
  FindMany(call: ServerDuplexStream<HeroById__Output, Hero>): void {},
  FindOne(
    call: ServerUnaryCall<HeroById__Output, Hero>,
    callback: sendUnaryData<Hero>,
  ): void {
    console.log('hey')
    callback(null, { id: 1, name: 'batman' })
  },
  // server handlers implementation...
}
