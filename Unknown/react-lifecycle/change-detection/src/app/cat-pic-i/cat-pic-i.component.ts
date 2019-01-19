/**
 * Cat Pic I component which uses default change detection
 * @author Andrew Jarombek
 * @since 1/17/2019
 */

import {ChangeDetectionStrategy, Component, EventEmitter, Input, Output} from '@angular/core';

@Component({
    selector: 'cat-pic-i',
    changeDetection: ChangeDetectionStrategy.Default,
    templateUrl: './cat-pic-i.component.html',
    styleUrls: ['./cat-pic-i.component.scss']
})
export class CatPicIComponent {

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
