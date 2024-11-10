import { Component, OnInit } from "@angular/core";
import { BehaviorSubject } from "rxjs";

@Component({
  selector: "about",
  templateUrl: "./about.component.html",
  styleUrls: ["./about.component.css"],
})
export class AboutComponent implements OnInit {
  ngOnInit() {
    // const subject = new Subject();
    const subject = new BehaviorSubject(0);

    // subject.complete()
    const series$ = subject.asObservable();

    series$.subscribe((val) => console.log("early : ", val));

    subject.next(1);
    subject.next(2);
    subject.next(3);

    subject.complete();

    setTimeout(() => {
      series$.subscribe((val) => console.log("lazily : ", val));

      subject.next(4);
      subject.next(5);
      subject.next(6);
    }, 3000);
  }
}
