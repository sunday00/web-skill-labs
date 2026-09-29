import { IQueryHandler, QueryHandler } from '@nestjs/cqrs'

// export class KillDragon extends Query<number> {
//   constructor(public readonly id: string) {
//     super()
//   }
// }

// export class KillDragon implements IQuery {
//   constructor(public readonly id: string) {}
// }

export class KillDragon {
  constructor(public readonly id: string) {}
}

@QueryHandler(KillDragon)
export class KillDragonHandler implements IQueryHandler<KillDragon> {
  async execute(query: KillDragon): Promise<number> {
    return 1
  }
}
