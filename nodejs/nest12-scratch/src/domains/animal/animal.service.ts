import { Injectable } from '@nestjs/common'
import { CommandBus } from '@nestjs/cqrs'
import { AnimalCreateCommand } from './handlers/animal.create.c.js'
import { DiscoveryService } from '@nestjs/core'
import { ListForDev } from '../../aop/decorators/discovable.decorator.js'

@Injectable()
export class AnimalService {
  constructor(
    private readonly cb: CommandBus,
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
}
