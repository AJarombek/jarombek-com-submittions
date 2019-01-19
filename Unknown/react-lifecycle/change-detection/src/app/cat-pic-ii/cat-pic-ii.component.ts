/**
 * Cat Pic II component which uses OnPush change detection
 * @author Andrew Jarombek
 * @since 1/17/2019
 */

import { ChangeDetectionStrategy, Component, EventEmitter, Input, Output } from '@angular/core';

@Component({
    selector: 'cat-pic-ii',
    changeDetection: ChangeDetectionStrategy.OnPush,
    templateUrl: './cat-pic-ii.component.html',
    styleUrls: ['./cat-pic-ii.component.scss']
})
export class CatPicIIComponent {

    @Input()
    count: number;

    @Input()
    immutableCount: object;

    @Output()
    change = new EventEmitter<object>();

    /**
     * Emit data when the count changes.  This method can be overridden in a parent component.
     */
    onChange() {
        this.count++;
        this.change.emit({count: this.count});
    }
}
