import { Injectable } from '@nestjs/common'
import { FirebaseService } from '../_common/modules/firebase/firebase.service'

@Injectable()
export class UFireBaseService {
  constructor(private readonly fb: FirebaseService) {}

  public async justGet() {
    const c = await this.fb.get3('ccc')

    // console.log(c) .

    return c.value
  }
}
