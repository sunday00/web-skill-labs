import { Component, OnInit } from '@angular/core'
import { Observable, of } from 'rxjs'
import { catchError, map, shareReplay, tap } from 'rxjs/operators'
import { createHttpObservable } from '../common/util'
import { Course } from '../model/course'

@Component({
  selector: 'home',
  templateUrl: './home.component.html',
  styleUrls: ['./home.component.css'],
})
export class HomeComponent implements OnInit {
  beginnerCourses$: Observable<Course[]>
  advancedCourses$: Observable<Course[]>

  constructor() {}

  ngOnInit() {
    const http$ = createHttpObservable('/api/courses')

    const courses$ = http$.pipe(
      tap(() => console.log('executed')),
      map((r) => Object.values(r['payload'])),
      shareReplay(),
      catchError((err) => of([])),
    )

    this.beginnerCourses$ = courses$.pipe(
      map((cs: Course[]) => cs.filter((c) => c.category === 'BEGINNER')),
    )
    this.advancedCourses$ = courses$.pipe(
      map((cs: Course[]) => cs.filter((c) => c.category === 'ADVANCED')),
    )

    // courses$.subscribe(
    //   (c: Course[]) => {
    //     // this is not rx
    //     // this.beginnerCourses = c.filter((cc) => cc.category === 'BEGINNER')
    //     // this.advancedCourses = c.filter((cc) => cc.category === 'ADVANCED')
    //   },
    //   noop,
    //   () => console.log('done'),
    // )
  }
}
