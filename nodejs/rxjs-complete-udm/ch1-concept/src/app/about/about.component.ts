import { Component, OnInit, ViewEncapsulation } from '@angular/core'
import { fromEvent, interval, noop, Observable, timer } from 'rxjs'

@Component({
  selector: 'about',
  templateUrl: './about.component.html',
  styleUrls: ['./about.component.css'],
})
export class AboutComponent implements OnInit {
  constructor() {}

  ngOnInit() {
    // document.addEventListener('click', (event) => {
    //   console.log(event)
    //
    //   setTimeout(() => {
    //     console.log('fin')
    //
    //     let counter = 0
    //     setInterval(() => {
    //       console.log(counter)
    //
    //       counter++
    //     }, 1000)
    //   }, 3000)
    // })
    // // const interval$ = interval(1000)
    // const interval$ = timer(3000, 1000)
    //
    // // interval$.subscribe((val) => console.log(`stream2 ${val}`))
    //
    // const click$ = fromEvent(document, 'click')
    // click$.subscribe(
    //   (evt) => {
    //     console.log(evt)
    //
    //     const sub = interval$.subscribe((val) => console.log(`stream1 ${val}`))
    //
    //     setTimeout(() => {
    //       sub.unsubscribe()
    //     }, 10000)
    //   }, // try
    //   (err) => console.log(err), // catch
    //   () => console.log('complete'), // finally
    // )

    const http$ = new Observable((observer) => {
      fetch('/api/courses')
        .then((res) => res.json())
        .then((body) => {
          observer.next(body)
          observer.complete()
        })
        .catch((error) => {
          observer.error(error)
        })
    })

    http$.subscribe(
      (c) => console.log(c),
      // () => {},
      noop,
      () => console.log('done'),
    )
  }
}
