import {
  AfterViewInit,
  Component,
  ElementRef,
  OnInit,
  ViewChild,
} from "@angular/core";
import { ActivatedRoute } from "@angular/router";
import { Course } from "../model/course";
import {
  debounceTime,
  distinctUntilChanged,
  map,
  switchMap,
  take,
  withLatestFrom,
} from "rxjs/operators";
import { concat, fromEvent, Observable } from "rxjs";
import { Lesson } from "../model/lesson";
import { createHttpObservable } from "../common/util";
import { Store } from "../common/store.service";

@Component({
  selector: "course",
  templateUrl: "./course.component.html",
  styleUrls: ["./course.component.css"],
})
export class CourseComponent implements OnInit, AfterViewInit {
  courseId: number;

  course$: Observable<Course>;
  // course: Course;

  lessons$: Observable<Lesson[]>;

  @ViewChild("searchInput", { static: true }) input: ElementRef;

  constructor(
    private route: ActivatedRoute,
    private store: Store,
  ) {}

  ngOnInit() {
    this.courseId = Number(this.route.snapshot.params["id"] ?? 0);

    // this.course$ = createHttpObservable(`/api/courses/${this.courseId}`);
    this.course$ = this.store.selectCourseById(this.courseId).pipe(
      // first()
      take(1),
    );

    // forkJoin(this.course$, this.loadLessons()).subscribe(console.log);

    // this.course$.subscribe((course) => {
    //   this.course = course;
    // });
    this.loadLessons()
      .pipe(withLatestFrom(this.course$))
      .subscribe(([lessons, course]) => {
        console.log(lessons);

        console.log(course);
      });
  }

  ngAfterViewInit() {
    const searchLessons$ = fromEvent<any>(
      this.input.nativeElement,
      "keyup",
    ).pipe(
      map((event) => event.target.value),
      debounceTime(400),
      distinctUntilChanged(),
      switchMap((search) => this.loadLessons(search)),
    );

    const initialLessons$ = this.loadLessons();

    this.lessons$ = concat(initialLessons$, searchLessons$);
  }

  loadLessons(search = ""): Observable<Lesson[]> {
    return createHttpObservable(
      `/api/lessons?courseId=${this.courseId}&pageSize=100&filter=${search}`,
    ).pipe(map((res) => res["payload"]));
  }
}
