import { AfterViewInit, Component, ElementRef, OnInit, ViewChild } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { Course } from '../model/course'
import { fromEvent, Observable } from 'rxjs'
import { Lesson } from '../model/lesson'
import { createHttpObservable } from '../common/util'
import { debounceTime, distinctUntilChanged, map, startWith, switchMap } from 'rxjs/operators'
import { debug, RxJsLoggingLevel, setRxJsLoggingLevel } from '../common/debug'

@Component({
  selector: 'course',
  templateUrl: './course.component.html',
  styleUrls: ['./course.component.css'],
})
export class CourseComponent implements OnInit, AfterViewInit {
  courseId: string

  course$: Observable<Course>
  lessons$: Observable<Lesson[]>

  @ViewChild('searchInput', { static: true }) input: ElementRef

  constructor(private route: ActivatedRoute) {}

  ngOnInit() {
    this.courseId = this.route.snapshot.params['id']

    this.course$ = createHttpObservable<Course>(`/api/courses/${this.courseId}`).pipe(
      // tap((search) => console.log({ search })),
      debug(RxJsLoggingLevel.INFO, 'course  value '),
    )

    setRxJsLoggingLevel(RxJsLoggingLevel.DEBUG)
  }

  ngAfterViewInit() {
    // const searchLessons$ = fromEvent(this.input.nativeElement, 'keyup').pipe(
    //   map((event: InputEvent) => (event.target as HTMLInputElement).value),
    //   debounceTime(500),
    //   distinctUntilChanged(),
    //   switchMap((search) => this.loadLessons(search)),
    // )
    //
    // const initialLessons$ = this.loadLessons()
    //
    // this.lessons$ = concat(initialLessons$, searchLessons$)

    this.lessons$ = fromEvent(this.input.nativeElement, 'keyup').pipe(
      map((event: InputEvent) => (event.target as HTMLInputElement).value),
      startWith(''),
      // tap((search) => console.log({ search })),
      debug(RxJsLoggingLevel.TRACE, 'search'),
      debounceTime(500),
      distinctUntilChanged(),
      switchMap((search) => this.loadLessons(search)),
      debug(RxJsLoggingLevel.DEBUG, 'lessons value'),
    )

    // fromEvent(this.input.nativeElement, 'keyup')
    //   .pipe(
    //     map((event: InputEvent) => (event.target as HTMLInputElement).value),
    //     startWith(''),
    //     debounceTime(500),
    //     // throttle(() => interval(500)),
    //     throttleTime(500),
    //   )
    //   .subscribe(console.log)
  }

  loadLessons(search = ''): Observable<Lesson[]> {
    return createHttpObservable(
      `/api/lessons?courseId=${this.courseId}&pageSize=100&filter=${search}`,
    ).pipe(map((res) => res['payload']))
  }
}
