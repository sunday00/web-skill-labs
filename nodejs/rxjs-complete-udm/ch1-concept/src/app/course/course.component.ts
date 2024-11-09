import { AfterViewInit, Component, ElementRef, OnInit, ViewChild } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { Course } from '../model/course'
import { fromEvent, Observable } from 'rxjs'
import { Lesson } from '../model/lesson'
import { createHttpObservable } from '../common/util'
import { debounceTime, distinctUntilChanged, map, startWith, switchMap } from 'rxjs/operators'

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

    this.course$ = createHttpObservable(`/api/courses/${this.courseId}`)
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
      debounceTime(500),
      distinctUntilChanged(),
      switchMap((search) => this.loadLessons(search)),
    )
  }

  loadLessons(search = ''): Observable<Lesson[]> {
    return createHttpObservable(
      `/api/lessons?courseId=${this.courseId}&pageSize=100&filter=${search}`,
    ).pipe(map((res) => res['payload']))
  }
}
