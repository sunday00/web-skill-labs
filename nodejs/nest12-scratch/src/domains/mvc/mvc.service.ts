import { Injectable } from '@nestjs/common'
import { type Response } from 'express'

@Injectable()
export class MvcService {
  async index() {
    return { message: 'hello' }
  }

  async responseWithView(res: Response, view: string) {
    return res.render(`mvc/${view}`, { name: 'kim' })
  }
}
