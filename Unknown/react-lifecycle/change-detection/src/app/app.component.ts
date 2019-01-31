/**
 * Main application component
 * @author Andrew Jarombek
 * @since 1/17/2019
 */

import { Component } from '@angular/core';

@Component({
    selector: 'app-root',
    templateUrl: './app.component.html',
    styleUrls: ['./app.component.scss']
})
export class AppComponent {

    // The component state passed to Horse-Pic-I
    // Both a primitive number type and an object type are passed to the component.  While the
    // reference to the primitive type changes every time the value changes, the reference to the
    // object type never changes.
    countI = 0;
    primitiveCountI = this.countI;
    objectCountI = {count: this.countI};

    /**
     * Overrides the onChange() method in Horse-Pic-I which responds to button clicks
     * @param {boolean} usingPrimitive - whether to change the primitive count or object count
     */
    onChangeI(usingPrimitive: boolean) {
        this.countI++;

        if (usingPrimitive) {
            this.primitiveCountI = this.countI;
        } else {
            this.objectCountI.count = this.countI;
        }
    }

    // The component state passed to Horse-Pic-II
    countII = 0;
    primitiveCountII = this.countII;
    objectCountII = {count: this.countII};

    /**
     * Overrides the onChange() method in Horse-Pic-II which responds to button clicks
     * @param {boolean} usingPrimitive - whether to change the primitive count or object count
     */
    onChangeII(usingPrimitive: boolean) {
        console.info("Invoked");
        this.countII++;

        if (usingPrimitive) {
            this.primitiveCountII = this.countII;
        } else {
            this.objectCountII.count = this.countII;
        }
        console.info(`Primitive: ${this.primitiveCountII}`)
        console.info(`Object: ${this.objectCountII.count}`)
    }
}
