import {
    AfterContentChecked,
    AfterContentInit, AfterViewChecked,
    AfterViewInit,
    Component,
    DoCheck,
    OnChanges,
    OnDestroy,
    OnInit
} from '@angular/core';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent implements OnChanges, OnInit, DoCheck, AfterContentInit,
    AfterContentChecked, AfterViewInit, AfterViewChecked, OnDestroy {
  title = 'app';

    ngOnChanges(): void {
        console.info("Inside AppComponent ngOnChanges");
    }

    ngOnInit(): void {
        console.info("Inside AppComponent ngOnInit");
    }

    ngDoCheck(): void {
        console.info("Inside AppComponent ngDoCheck");
    }

    ngAfterContentInit(): void {
        console.info("Inside AppComponent ngAfterContentInit");
    }

    ngAfterContentChecked(): void {
        console.info("Inside AppComponent ngAfterContentChecked");
    }

    ngAfterViewInit(): void {
        console.info("Inside AppComponent ngAfterViewInit");
    }

    ngAfterViewChecked(): void {
        console.info("Inside AppComponent ngAfterViewChecked");
    }

    ngOnDestroy(): void {
        console.info("Inside AppComponent ngOnDestroy");
    }
}
