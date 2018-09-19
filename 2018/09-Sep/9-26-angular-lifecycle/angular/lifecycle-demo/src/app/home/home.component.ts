import { Component, OnInit } from '@angular/core';

@Component({
    selector: 'app-home',
    templateUrl: './home.component.html',
    styleUrls: ['./home.component.css']
})
export class HomeComponent implements OnInit {

    lifeCycles: [any];

    private LOG_TAG: string = '[Home.Component]';

    constructor() { }

    ngOnInit() {
        console.info(`${this.LOG_TAG} Inside ngOnInit`);
    }
}
