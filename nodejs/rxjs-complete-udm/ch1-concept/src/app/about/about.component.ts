import { Component, OnInit } from '@angular/core'

@Component({
  selector: 'about',
  templateUrl: './about.component.html',
  styleUrls: ['./about.component.css'],
})
export class AboutComponent implements OnInit {
  constructor() {}

  ngOnInit() {
    // const interval1$ = interval(3000)
    // const interval2$ = interval1$.pipe(map((v) => 10 * v))
    //
    // const result$ = merge(interval1$, interval2$)
    //
    // result$.subscribe(console.log)
  }
}
