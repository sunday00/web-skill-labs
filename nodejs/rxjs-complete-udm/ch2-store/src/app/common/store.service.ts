import { Injectable } from "@angular/core";
import { BehaviorSubject, Observable } from "rxjs";
import { Course } from "../model/course";
import { createHttpObservable } from "./util";
import { filter, map, tap } from "rxjs/operators";
import { fromPromise } from "rxjs/internal-compatibility";

@Injectable({
  providedIn: "root",
})
export class Store {
  private subject = new BehaviorSubject<Course[]>([]);

  courses$: Observable<Course[]> = this.subject.asObservable();

  init() {
    const http$ = createHttpObservable("/api/courses");
    const courses$: Observable<Course[]> = http$
      .pipe(
        tap(() => console.log("HTTP request executed")),
        map((res) => Object.values(res["payload"])),
        // shareReplay(),
        // retryWhen((errors) => errors.pipe(delayWhen(() => timer(2000)))),
      )
      .subscribe((courses: Course[]) => this.subject.next(courses));
  }

  selectBeginnerCourses() {
    return this.filterByCategory("BEGINNER");
  }

  selectAdvancedCourses() {
    return this.filterByCategory("ADVANCED");
  }

  filterByCategory(category: string) {
    return this.courses$.pipe(
      map((courses) => courses.filter((course) => course.category == category)),
    );
  }

  saveCourse(id: number, value: Course): Observable<any> {
    const courses = this.subject.getValue();
    const courseIndex = courses.findIndex((courses) => courses.id === id);

    const newCourses = courses.slice(0);
    newCourses[courseIndex] = {
      ...courses[courseIndex],
      ...value,
    };

    this.subject.next(newCourses);

    return fromPromise(
      fetch(`/api/courses/${id}`, {
        method: "PUT",
        body: JSON.stringify(newCourses),
        headers: { "Content-Type": "application/json" },
      }),
    );
  }

  selectCourseById(courseId: number): Observable<Course> {
    return this.courses$.pipe(
      map((courses) =>
        courses.find((course) => {
          return course.id === courseId;
        }),
      ),
      filter((course) => !!course),
    );
  }
}
