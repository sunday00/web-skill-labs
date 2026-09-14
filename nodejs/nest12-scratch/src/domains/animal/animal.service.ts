import { Injectable } from '@nestjs/common'
import { CommandBus, QueryBus } from '@nestjs/cqrs'
import { AnimalCreateCommand } from './handlers/animal.create.c.js'
import { DiscoveryService } from '@nestjs/core'
import { ListForDev } from '../../aop/decorators/discovable.decorator.js'
import { AnimalSampleContentQ } from './handlers/animal.sample.content.q.js'
import { AnimalCacheBurstC } from './handlers/animal.cache.burst.c.js'

@Injectable()
export class AnimalService {
  constructor(
    private readonly cb: CommandBus,
    private readonly qb: QueryBus,
    private readonly discover: DiscoveryService,
  ) {}

  async create(data: AnimalCreateCommand) {
    return await this.cb.execute(data)
  }

  async errFromController() {
    throw new Error('OOOOOOOPPPPPPPSSSS!!!!')
  }

  async useDiscover() {
    const controller = this.discover.getControllers()
    const providers = this.discover.getProviders()

    providers.forEach((provider) => {
      const decorators = this.discover.getMetadataByDecorator(
        ListForDev,
        provider,
      )

      decorators ? console.log('Discover Tagged: ' + decorators) : ''
    })

    console.log(controller.map((c) => c.name))
    console.log(providers.map((c) => c.name))

    return 1
  }

  async useCache() {
    return await this.qb.execute(new AnimalSampleContentQ())
  }

  async burstCache() {
    return await this.cb.execute(new AnimalCacheBurstC())
  }
}
