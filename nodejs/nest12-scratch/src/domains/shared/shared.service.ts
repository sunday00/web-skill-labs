import { Injectable } from '@nestjs/common'
import { EventEmitter2, OnEvent } from '@nestjs/event-emitter'

@Injectable()
export class SharedService {
  private no = 0

  constructor(private emitter: EventEmitter2) {}

  async getNo() {
    return this.no++
  }

  @OnEvent('from.animal', { prependListener: false /* async: true */ })
  public async eventListen(payload: { name: string }) {
    console.log(payload)

    const res = payload.name + ' proceed'

    return res
  }

  @OnEvent('from.animal', { prependListener: false /* async: true */ })
  public async eventListen2(payload: { name: string }) {
    console.log(payload)

    const res = payload.name + ' persisted'

    return res
  }

  @OnEvent('from.wild.ch1', { prependListener: false })
  public async eventListen3(payload: { name: string }) {
    return payload.name + ' ch1'
  }

  @OnEvent('from.wild.ch2', { prependListener: false })
  public async eventListen4(payload: { name: string }) {
    return payload.name + ' ch2'
  }

  @OnEvent('from.wild.ch3', { prependListener: false })
  public async eventListen5(payload: { name: string }) {
    return payload.name + ' ch3'
  }

  @OnEvent('from.wild.ch4', { prependListener: false })
  public async eventListen6(payload: { name: string }) {
    return payload.name + ' ch4'
  }
}
