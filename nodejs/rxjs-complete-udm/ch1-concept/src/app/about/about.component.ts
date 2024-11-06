import { Component, OnInit, ViewEncapsulation } from '@angular/core'
import { fromEvent, interval, noop, Observable, timer } from 'rxjs'
import { createHttpObservable } from '../common/util'
import { map } from 'rxjs/operators'

@Component({
  selector: 'about',
  templateUrl: './about.component.html',
  styleUrls: ['./about.component.css'],
})
export class AboutComponent implements OnInit {
  constructor() {}

  ngOnInit() {
    const http$ = createHttpObservable('/api/courses')

    const courses$ = http$.pipe(map((r) => Object.values(r['payload'])))

    courses$.subscribe(
      (c) => console.log(c),
      // () => {},
      noop,
      () => console.log('done'),
    )
  }
}
