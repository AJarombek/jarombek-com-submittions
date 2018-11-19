import {Component, Input, OnChanges, OnInit} from '@angular/core';
import {Lifecycle} from "../lifecycle";
import {LifecycleService} from "../lifecycle.service";

/**
 * Notification component which displays lifecycle notifications
 * @author Andrew Jarombek
 * @since 9/19/2018
 */

@Component({
  selector: 'app-notification',
  templateUrl: './notification.component.html',
  styleUrls: ['./notification.component.css']
})
export class NotificationComponent implements OnChanges, OnInit {

    @Input() content: Lifecycle;

    private LOG_TAG: string = '[Notification.Component]';

    /**
     * {@link AppComponent#constructor}
     */
    constructor(private lifecycleService: LifecycleService) { }

    /**
     * {@link AppComponent#ngOnChanges}
     */
    ngOnChanges(): void {
        console.info(`${this.LOG_TAG} Inside ngOnChanges`);
        console.info(this.content);
    }

    /**
     * {@link AppComponent#ngOnInit}
     */
    ngOnInit() {
        console.info(`${this.LOG_TAG} Inside ngOnInit`);
    }
}
