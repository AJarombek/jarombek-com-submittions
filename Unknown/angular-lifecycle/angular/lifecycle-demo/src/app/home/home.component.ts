import {Component, OnInit, OnDestroy} from '@angular/core';
import {LifecycleService} from "../lifecycle.service";
import {delay, takeUntil} from "rxjs/operators";
import {Subject} from "rxjs/Subject";
import {Lifecycle} from "../lifecycle";

/**
 * @author Andrew Jarombek
 * @since 9/19/2018
 */

@Component({
    selector: 'app-home',
    templateUrl: './home.component.html',
    styleUrls: ['./home.component.css']
})
export class HomeComponent implements OnInit, OnDestroy {

    private ngUnsubscribe: Subject<any> = new Subject<any>();
    private LOG_TAG: string = '[Home.Component]';
    lifecycleList: Lifecycle[];

    /**
     * {@link AppComponent#constructor}
     */
    constructor(private lifecycleService: LifecycleService) { }

    /**
     * {@link AppComponent#ngOnInit}
     */
    ngOnInit() {
        console.info(`${this.LOG_TAG} Inside ngOnInit`);

        this.lifecycleList = [
            {
                component: this.LOG_TAG,
                lifecycle: 'ngOnInit',
                type: 'Initialize'
            }
        ];

        // Why is delay(0) needed?  https://bit.ly/2A3UPXR
        this.lifecycleService.onData
                .pipe(
                    takeUntil(this.ngUnsubscribe),
                    delay(0))
                .subscribe(res => {
                    console.info(`Message Received: ${JSON.stringify(res)}`);

                    this.lifecycleList = [
                        res,
                        ...this.lifecycleList
                    ];
                });
    }

    /**
     * {@link AppComponent#ngOnDestroy}
     */
    ngOnDestroy(): void {
        console.info(`${this.LOG_TAG} Inside ngOnDestroy`);
        this.ngUnsubscribe.next();
        this.ngUnsubscribe.complete();
    }
}
