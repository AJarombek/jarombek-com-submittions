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

    // The component state passed to Cat-Pic-I
    countI = 0;
    immutableCountI = {count: this.countI};

    /**
     * Overrides the onChange() method in Cat-Pic-I which responds to button clicks
     */
    onChangeI() {
        this.countI++;
    }

    // The component state passed to Cat-Pic-II
    countII = 0;
    immutableCountII = {count: this.countII};

    /**
     * Overrides the onChange() method in Cat-Pic-II which responds to button clicks
     */
    onChangeII() {
        this.countII++;
    }
}
