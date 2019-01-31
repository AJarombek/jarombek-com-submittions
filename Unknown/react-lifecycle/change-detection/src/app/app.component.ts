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
    primitiveCountI = 0;
    objectCountI = {count: this.primitiveCountI};

    /**
     * Overrides the onChange() method in Horse-Pic-I which responds to button clicks
     */
    onChangeI() {
        this.primitiveCountI++;
        this.objectCountI.count = this.primitiveCountI;
    }

    // The component state passed to Horse-Pic-II
    primitiveCountII = 0;
    objectCountII = {count: this.primitiveCountII};

    /**
     * Overrides the onChange() method in Horse-Pic-II which responds to button clicks
     */
    onChangeII() {
        this.primitiveCountII++;
        this.objectCountII.count = this.primitiveCountII;
    }
}
