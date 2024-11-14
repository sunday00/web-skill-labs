import { AfterViewInit, Component, ElementRef, Inject, OnInit, ViewChild } from '@angular/core'
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog'
import { Course } from '../model/course'
import { FormBuilder, FormGroup, Validators } from '@angular/forms'
import moment from 'moment'
import { exhaustMap, filter, mergeMap } from 'rxjs/operators'
import { fromPromise } from 'rxjs/internal-compatibility'
import { fromEvent } from 'rxjs'

@Component({
  selector: 'course-dialog',
  templateUrl: './course-dialog.component.html',
  styleUrls: ['./course-dialog.component.css'],
})
export class CourseDialogComponent implements OnInit, AfterViewInit {
  form: FormGroup
  course: Course

  @ViewChild('saveButton', { static: true }) saveButton: ElementRef

  @ViewChild('searchInput', { static: true }) searchInput: ElementRef

  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<CourseDialogComponent>,
    @Inject(MAT_DIALOG_DATA) course: Course,
  ) {
    this.course = course

    this.form = fb.group({
      description: [course.description, Validators.required],
      category: [course.category, Validators.required],
      releasedAt: [moment(), Validators.required],
      longDescription: [course.longDescription, Validators.required],
    })
  }

  ngOnInit() {
    this.form.valueChanges
      .pipe(
        filter(() => this.form.valid),
        // concatMap((changes) => this.saveCourses(changes)),
        mergeMap((changes) => this.saveCourses(changes)),
      )
      .subscribe()
  }

  saveCourses(changes) {
    return fromPromise(
      fetch(`/api/courses/${this.course.id}`, {
        method: 'PUT',
        body: JSON.stringify(changes),
        headers: {
          'Content-Type': 'application/json',
        },
      }),
    )
  }

  ngAfterViewInit() {
    fromEvent(this.saveButton.nativeElement, 'click')
      .pipe(
        // concatMap(() => this.saveCourses(this.form.value)),
        exhaustMap(() => this.saveCourses(this.form.value)),
      )
      .subscribe()
  }

  close() {
    this.dialogRef.close()
  }

  save() {}
}
