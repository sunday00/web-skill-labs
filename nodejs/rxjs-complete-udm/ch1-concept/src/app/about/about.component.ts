import { Component, OnInit } from '@angular/core'
import { asyncScheduler, of, scheduled } from 'rxjs'
import { concatAll } from 'rxjs/operators'

@Component({
  selector: 'about',
  templateUrl: './about.component.html',
  styleUrls: ['./about.component.css'],
})
export class AboutComponent implements OnInit {
  constructor() {}

  ngOnInit() {
    const source1$ = of(1, 2, 3)
    // const source1$ = interval(1000)
    const source2$ = of(4, 5, 6)
    const source3$ = of(7, 8, 9)

    // const r$ = concat(source1$, source2$, source3$)
    const r$ = scheduled([source1$, source2$, source3$], asyncScheduler).pipe(concatAll())
    r$.subscribe(console.log)
  }
}
