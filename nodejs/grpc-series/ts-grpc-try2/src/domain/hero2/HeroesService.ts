// Original file: resources/grpc/domain/hero2/hero.proto

import type * as grpc from '@grpc/grpc-js'
import type { MethodDefinition } from '@grpc/proto-loader'
import type { Hero as _hero2_Hero, Hero__Output as _hero2_Hero__Output } from '../hero2/Hero';
import type { HeroById as _hero2_HeroById, HeroById__Output as _hero2_HeroById__Output } from '../hero2/HeroById';

export interface HeroesServiceClient extends grpc.Client {
  FindMany(metadata: grpc.Metadata, options?: grpc.CallOptions): grpc.ClientDuplexStream<_hero2_HeroById, _hero2_Hero__Output>;
  FindMany(options?: grpc.CallOptions): grpc.ClientDuplexStream<_hero2_HeroById, _hero2_Hero__Output>;
  findMany(metadata: grpc.Metadata, options?: grpc.CallOptions): grpc.ClientDuplexStream<_hero2_HeroById, _hero2_Hero__Output>;
  findMany(options?: grpc.CallOptions): grpc.ClientDuplexStream<_hero2_HeroById, _hero2_Hero__Output>;
  
  FindOne(argument: _hero2_HeroById, metadata: grpc.Metadata, options: grpc.CallOptions, callback: grpc.requestCallback<_hero2_Hero__Output>): grpc.ClientUnaryCall;
  FindOne(argument: _hero2_HeroById, metadata: grpc.Metadata, callback: grpc.requestCallback<_hero2_Hero__Output>): grpc.ClientUnaryCall;
  FindOne(argument: _hero2_HeroById, options: grpc.CallOptions, callback: grpc.requestCallback<_hero2_Hero__Output>): grpc.ClientUnaryCall;
  FindOne(argument: _hero2_HeroById, callback: grpc.requestCallback<_hero2_Hero__Output>): grpc.ClientUnaryCall;
  findOne(argument: _hero2_HeroById, metadata: grpc.Metadata, options: grpc.CallOptions, callback: grpc.requestCallback<_hero2_Hero__Output>): grpc.ClientUnaryCall;
  findOne(argument: _hero2_HeroById, metadata: grpc.Metadata, callback: grpc.requestCallback<_hero2_Hero__Output>): grpc.ClientUnaryCall;
  findOne(argument: _hero2_HeroById, options: grpc.CallOptions, callback: grpc.requestCallback<_hero2_Hero__Output>): grpc.ClientUnaryCall;
  findOne(argument: _hero2_HeroById, callback: grpc.requestCallback<_hero2_Hero__Output>): grpc.ClientUnaryCall;
  
}

export interface HeroesServiceHandlers extends grpc.UntypedServiceImplementation {
  FindMany: grpc.handleBidiStreamingCall<_hero2_HeroById__Output, _hero2_Hero>;
  
  FindOne: grpc.handleUnaryCall<_hero2_HeroById__Output, _hero2_Hero>;
  
}

export interface HeroesServiceDefinition extends grpc.ServiceDefinition {
  FindMany: MethodDefinition<_hero2_HeroById, _hero2_Hero, _hero2_HeroById__Output, _hero2_Hero__Output>
  FindOne: MethodDefinition<_hero2_HeroById, _hero2_Hero, _hero2_HeroById__Output, _hero2_Hero__Output>
}
