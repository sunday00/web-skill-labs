import { Injectable } from '@nestjs/common'
import { CommandBus, QueryBus } from '@nestjs/cqrs'
import { AnimalCreateCommand } from './handlers/animal.create.c.js'
import { DiscoveryService } from '@nestjs/core'
import { ListForDev } from '../../aop/decorators/discovable.decorator.js'
import { AnimalSampleContentQ } from './handlers/animal.sample.content.q.js'
import { AnimalCacheBurstC } from './handlers/animal.cache.burst.c.js'
import { SchedulerRegistry } from '@nestjs/schedule'

@Injectable()
export class AnimalService {
  tName = 'DynamicTimeoutDelay1'

  constructor(
    private readonly cb: CommandBus,
    private readonly qb: QueryBus,
    private readonly discover: DiscoveryService,
    private readonly schedulerRegistry: SchedulerRegistry,
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

  async useDelay(seconds: number) {
    const to = setTimeout(() => {
      console.log('Oh~ delayed~!!')
      this.schedulerRegistry.deleteTimeout(this.tName)
    }, seconds * 1000)

    this.schedulerRegistry.addTimeout(this.tName, to)

    return true
  }
}
