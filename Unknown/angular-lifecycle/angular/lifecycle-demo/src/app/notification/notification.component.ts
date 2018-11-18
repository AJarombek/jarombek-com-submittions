import {Component, Input, OnChanges, OnInit} from '@angular/core';
import {Lifecycle} from "../lifecycle";

/**
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

    constructor() { }

    ngOnChanges(): void {
        console.info(`${this.LOG_TAG} Inside ngOnChanges`);
    }

    ngOnInit() {
        console.info(`${this.LOG_TAG} Inside ngOnInit`);
    }

}
