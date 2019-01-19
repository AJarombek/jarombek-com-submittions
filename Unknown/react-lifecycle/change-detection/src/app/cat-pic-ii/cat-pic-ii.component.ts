/**
 * Cat Pic II component which uses OnPush change detection
 * @author Andrew Jarombek
 * @since 1/17/2019
 */

import {ChangeDetectionStrategy, Component, OnInit} from '@angular/core';

@Component({
    selector: 'cat-pic-ii',
    changeDetection: ChangeDetectionStrategy.OnPush,
    templateUrl: './cat-pic-ii.component.html',
    styleUrls: ['./cat-pic-ii.component.scss']
})
export class CatPicIIComponent implements OnInit {

    constructor() { }

    ngOnInit() {

    }
}
