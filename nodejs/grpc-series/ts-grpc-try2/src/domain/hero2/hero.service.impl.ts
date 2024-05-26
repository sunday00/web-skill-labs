import { HeroesServiceHandlers } from './HeroesService'
import {
  sendUnaryData,
  ServerDuplexStream,
  ServerUnaryCall,
} from '@grpc/grpc-js'
import { HeroById__Output } from './HeroById'
import { Hero } from './Hero'
import * as protoLoader from '@grpc/proto-loader'
import * as grpc from '@grpc/grpc-js'
import { ProtoGrpcType } from '../hero'

const packageDefinition = protoLoader.loadSync(
  './resources/grpc/domain/hero2/hero.proto',
)

export const heroProto2 = grpc.loadPackageDefinition(
  packageDefinition,
) as unknown as ProtoGrpcType

export const heroService2: HeroesServiceHandlers = {
  FindMany(call: ServerDuplexStream<HeroById__Output, Hero>): void {},
  FindOne(
    call: ServerUnaryCall<HeroById__Output, Hero>,
    callback: sendUnaryData<Hero>,
  ): void {
    console.log('hey2')
    callback(null, { id: 1, name: 'batman' })
  },
  // server handlers implementation...
}
