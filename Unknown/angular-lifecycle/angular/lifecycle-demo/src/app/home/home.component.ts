import {Component, OnInit, OnDestroy} from '@angular/core';
import {LifecycleService} from "../lifecycle.service";
import {takeUntil} from "rxjs/operators";
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
    public lifecycleList: Array<Lifecycle> = [];

    constructor(private lifecycleService: LifecycleService) { }

    ngOnInit() {
        console.info(`${this.LOG_TAG} Inside ngOnInit`);

        // Why is setTimeout() needed?  https://bit.ly/2A3UPXR
        setTimeout(() => {
            this.lifecycleService.onData.pipe(takeUntil(this.ngUnsubscribe)).subscribe(res => {
                console.info(`Message Received: ${JSON.stringify(res)}`);

                this.lifecycleList = [
                    res,
                    ...this.lifecycleList
                ]
            });
        });
    }

    ngOnDestroy(): void {
        console.info(`${this.LOG_TAG} Inside ngOnDestroy`);
        this.ngUnsubscribe.next();
        this.ngUnsubscribe.complete();
    }
}
