import type * as grpc from '@grpc/grpc-js';
import type { MessageTypeDefinition } from '@grpc/proto-loader';

import type { HeroesServiceClient as _hero_HeroesServiceClient, HeroesServiceDefinition as _hero_HeroesServiceDefinition } from './hero/HeroesService';
import type { HeroesServiceClient as _hero2_HeroesServiceClient, HeroesServiceDefinition as _hero2_HeroesServiceDefinition } from './hero2/HeroesService';

type SubtypeConstructor<Constructor extends new (...args: any) => any, Subtype> = {
  new(...args: ConstructorParameters<Constructor>): Subtype;
};

export interface ProtoGrpcType {
  hero: {
    Hero: MessageTypeDefinition
    HeroById: MessageTypeDefinition
    HeroesService: SubtypeConstructor<typeof grpc.Client, _hero_HeroesServiceClient> & { service: _hero_HeroesServiceDefinition }
  }
  hero2: {
    Hero: MessageTypeDefinition
    HeroById: MessageTypeDefinition
    HeroesService: SubtypeConstructor<typeof grpc.Client, _hero2_HeroesServiceClient> & { service: _hero2_HeroesServiceDefinition }
  }
}

