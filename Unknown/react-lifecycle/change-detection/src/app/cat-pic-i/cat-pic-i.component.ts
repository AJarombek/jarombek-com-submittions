/**
 * Cat Pic I component which uses default change detection
 * @author Andrew Jarombek
 * @since 1/17/2019
 */

import {ChangeDetectionStrategy, Component, OnInit} from '@angular/core';

@Component({
    selector: 'cat-pic-i',
    changeDetection: ChangeDetectionStrategy.Default,
    templateUrl: './cat-pic-i.component.html',
    styleUrls: ['./cat-pic-i.component.scss']
})
export class CatPicIComponent implements OnInit {

    constructor() { }

    ngOnInit() {
    }
}
