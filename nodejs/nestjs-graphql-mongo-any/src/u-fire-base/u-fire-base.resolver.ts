import { Query, Resolver } from '@nestjs/graphql'
import { UFireBaseService } from './u-fire-base.service'

@Resolver()
export class UFireBaseResolver {
  constructor(private readonly uFireBaseService: UFireBaseService) {}

  @Query(() => String, { name: 'simpleRC' })
  public async justGet() {
    return this.uFireBaseService.justGet()
  }
}
