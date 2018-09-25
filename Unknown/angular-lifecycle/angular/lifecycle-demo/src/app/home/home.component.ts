import {Component, Input, OnInit} from '@angular/core';

/**
 * @author Andrew Jarombek
 * @since 9/19/2018
 */

@Component({
    selector: 'app-home',
    templateUrl: './home.component.html',
    styleUrls: ['./home.component.css']
})
export class HomeComponent implements OnInit {

    @Input() lifecycles: [object];

    private LOG_TAG: string = '[Home.Component]';

    constructor() { }

    ngOnInit() {
        console.info(`${this.LOG_TAG} Inside ngOnInit`);
    }
}
