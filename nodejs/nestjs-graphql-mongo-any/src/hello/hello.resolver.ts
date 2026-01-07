import { Query, Resolver } from '@nestjs/graphql'
import { HelloService } from './hello.service'

@Resolver()
export class HelloResolver {
  constructor(private readonly helloService: HelloService) {}

  @Query(() => String, { name: 'simpleHello' })
  public async justHello() {
    return this.helloService.sayHello()
  }

  @Query(() => String, { name: 'simpleJwtWorkingTest' })
  public getFakeAccessToken() {
    return this.helloService.getFakeAccessToken()
  }
}
