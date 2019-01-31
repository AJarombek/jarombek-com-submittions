/**
 * Horse Pic I component which uses default change detection
 * @author Andrew Jarombek
 * @since 1/17/2019
 */

import {ChangeDetectionStrategy, Component, EventEmitter, Input, Output} from '@angular/core';

@Component({
    selector: 'horse-pic-i',
    // Horse-Pic-I uses Default change detection, while Horse-Pic-II uses OnPush change detection.
    changeDetection: ChangeDetectionStrategy.Default,
    templateUrl: './horse-pic-i.component.html',
    styleUrls: ['./horse-pic-i.component.scss']
})
export class HorsePicIComponent {

    // URL to the horse image for the component
    imageURL: string = "https://asset.jarombek.com/posts/1-29-19-horse-picture-1.jpg";

    // Determine if the component displays the times clicked using 'primitiveCount'.  If the
    // component isn't using 'primitiveCount', it uses 'objectCount'.
    usingPrimitive: boolean = true;

    // True for a short interval after a change occurs, false otherwise
    changeDetected: boolean = false;

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

    /**
     * Toggle the usingPrimitive flag
     */
    changeCountType() {
        this.usingPrimitive = !this.usingPrimitive;
    }

    /**
     * Toggle the changeDetected flag for a short interval.  changeDetected=true adds a CSS class
     * to the image.
     */
    addOnChangeClass() {
        this.changeDetected = true;
        setTimeout(() => this.changeDetected = false, 700)
    }

    /**
     * Getter for a runChangeDetection property.  This getter is invoked every time a change
     * detection cycle occurs in this component.
     * Source: https://bit.ly/2Uw9QKS
     * @return {boolean} - always returns true
     */
    get runChangeDetection() {
        console.info("Running Change Detection in HorsePicI");
        return true;
    }
}
