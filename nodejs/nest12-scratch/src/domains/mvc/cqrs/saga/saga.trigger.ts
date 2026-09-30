import { IEvent, ofType, Saga } from '@nestjs/cqrs'
import { Injectable } from '@nestjs/common'
import { map, Observable } from 'rxjs'
import { SagaStep1 } from './saga.step1.js'
import { SagaStep2 } from './saga.step2.js'
import { SagaStep3 } from './saga.step3.js'
import { SagaStep4 } from './saga.step4.js'

export class SagaTrigger implements IEvent {
  constructor(public name: string) {}
}

export class SagaStep1Done implements IEvent {
  constructor(public name: string) {}
}

export class SagaStep2Done implements IEvent {
  constructor(public name: string) {}
}

export class SagaStep3Done implements IEvent {
  constructor(public name: string) {}
}

export class SagaStep4Done implements IEvent {
  constructor(public name: string) {}
}

// @EventsHandler(SagaTrigger)
@Injectable()
export class SagaMain {
  // @Saga()
  // sagaIndex = (events$: Observable<any>) => {
  //   return events$.pipe(
  //     ofType(SagaTrigger),
  //     concatMap((event) =>
  //       from([
  //         new SagaStep1(event.name),
  //         new SagaStep2(event.name),
  //         new SagaStep3(event.name),
  //         new SagaStep4(event.name),
  //       ]),
  //     ),
  //   )
  // }

  @Saga()
  sagaIndex_step1 = (events$: Observable<any>) => {
    return events$.pipe(
      ofType(SagaTrigger),
      map((event) => new SagaStep1(event.name)),
    )
  }

  @Saga()
  sagaStep2 = (events$: Observable<any>) => {
    return events$.pipe(
      ofType(SagaStep1Done),
      map((event) => new SagaStep2(event.name)),
    )
  }

  @Saga()
  sagaStep3 = (events$: Observable<any>) => {
    return events$.pipe(
      ofType(SagaStep2Done),
      map((event) => new SagaStep3(event.name)),
    )
  }

  @Saga()
  sagaStep4 = (events$: Observable<any>) => {
    return events$.pipe(
      ofType(SagaStep3Done),
      map((event) => new SagaStep4(event.name)),
    )
  }
}
