import * as protoLoader from '@grpc/proto-loader'
import {
  sendUnaryData,
  ServerDuplexStream,
  ServerUnaryCall,
  loadPackageDefinition,
} from '@grpc/grpc-js'

import { HeroById__Output } from './HeroById' //req
import { Hero } from './Hero' // res
import { HeroesServiceHandlers } from './HeroesService'
import { ProtoGrpcType } from '../hero'

const packageDefinition = protoLoader.loadSync(
  './resources/grpc/domain/hero/hero.proto',
)
export const heroProto = loadPackageDefinition(
  packageDefinition,
) as unknown as ProtoGrpcType

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
