/**
 * Horse Pic II component which uses OnPush change detection
 * @author Andrew Jarombek
 * @since 1/17/2019
 */

import { ChangeDetectionStrategy, Component, EventEmitter, Input, Output } from '@angular/core';

@Component({
    selector: 'horse-pic-ii',
    // Horse-Pic-II uses OnPush change detection, while Horse-Pic-I uses Default change detection.
    changeDetection: ChangeDetectionStrategy.OnPush,
    templateUrl: './horse-pic-ii.component.html',
    styleUrls: ['./horse-pic-ii.component.scss']
})
export class HorsePicIIComponent {

    // URL to the horse image for the component
    imageURL: string = "https://asset.jarombek.com/posts/1-29-19-horse-picture-2.jpg";

    // Determine if the component displays the times clicked using 'primitiveCount'.  If the
    // component isn't using 'primitiveCount', it uses 'objectCount'.
    usingPrimitive: boolean = true;

    @Input()
    primitiveCount: number;

    @Input()
    objectCount: object;

    @Output()
    change = new EventEmitter<object>();

    /**
     * Emit data when the count changes.  This method can be overridden in a parent component.
     */
    onChange() {
        this.primitiveCount++;
        this.change.emit({count: this.primitiveCount});
    }
}
