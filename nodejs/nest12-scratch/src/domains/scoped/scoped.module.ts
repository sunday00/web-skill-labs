import { Module } from '@nestjs/common'
import { ScopedDefaultService } from './scoped.default.service.js'
import { ScopedRequestService } from './scoped.request.service.js'
import { ScopedTransientService } from './scoped.transient.service.js'
import { ScopedSdController } from './scoped.sd.controller.js'
import { ScopedSrController } from './scoped.sr.controller.js'
import { ScopedTrController } from './scoped.st.controller.js'
import { ScopedDefault1Consumer } from './scoped.consumer.default.1.js'
import { ScopedRequest1Consumer } from './scoped.consumer.request.1.js'
import { ScopedTransient1Consumer } from './scoped.consumer.transient.1.js'
import { ScopedDefault3Consumer } from './scoped.consumer.default.3.js'
import { ScopedDefault2Consumer } from './scoped.consumer.default.2.js'
import { ScopedRequest2Consumer } from './scoped.consumer.request.2.js'
import { ScopedRequest3Consumer } from './scoped.consumer.request.3.js'
import { ScopedTransient3Consumer } from './scoped.consumer.transient.3.js'
import { ScopedTransient2Consumer } from './scoped.consumer.transient.2.js'

@Module({
  controllers: [ScopedSdController, ScopedSrController, ScopedTrController],
  providers: [
    ScopedDefaultService,
    ScopedRequestService,
    ScopedTransientService,

    ScopedDefault1Consumer,
    ScopedDefault2Consumer,
    ScopedDefault3Consumer,

    ScopedRequest1Consumer,
    ScopedRequest2Consumer,
    ScopedRequest3Consumer,

    ScopedTransient1Consumer,
    ScopedTransient2Consumer,
    ScopedTransient3Consumer,
  ],
})
export class ScopedModule {}
