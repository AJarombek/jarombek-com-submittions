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

    static max = 5;
    static lifecycleCount = {
        ngOnChanges: NotificationComponent.max,
        ngOnInit: NotificationComponent.max
    };

    /**
     * {@link AppComponent#constructor}
     */
    constructor(private lifecycleService: LifecycleService) { }

    /**
     * {@link AppComponent#ngOnChanges}
     */
    ngOnChanges(): void {
        if (NotificationComponent.lifecycleCount.ngOnChanges > 0) {
            console.info(`${this.LOG_TAG} Inside ngOnChanges`);

            const lifecycle = {
                component: this.LOG_TAG,
                lifecycle: 'ngOnChanges',
                type: 'Change',
                message: `Notification Content: ${JSON.stringify(this.content)}`
            };

            this.lifecycleService.emitData(lifecycle);

            NotificationComponent.decrementLifecycleCount('ngOnChanges');
        }
    }

    /**
     * {@link AppComponent#ngOnInit}
     */
    ngOnInit() {
        if (NotificationComponent.lifecycleCount.ngOnInit > 0) {
            console.info(`${this.LOG_TAG} Inside ngOnInit`);

            const lifecycle = {
                component: this.LOG_TAG,
                lifecycle: 'ngOnInit',
                type: 'Initialize'
            };

            this.lifecycleService.emitData(lifecycle);

            NotificationComponent.decrementLifecycleCount('ngOnInit');
        }
    }

    /**
     * Update lifecycle count by decrementing the corresponding lifecycle property
     * @param {string} lifecycle - the name of the lifecycle method to decrement
     */
    static decrementLifecycleCount(lifecycle: string) {
        NotificationComponent.lifecycleCount = {
            ...NotificationComponent.lifecycleCount,
            [`${lifecycle}`]: NotificationComponent.lifecycleCount[`${lifecycle}`] - 1
        };

        console.info(JSON.stringify(NotificationComponent.lifecycleCount));
    }
}
