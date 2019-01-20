/**
 * Configure the module for the application
 * @author Andrew Jarombek
 * @since 1/17/2019
 */

import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';

import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { HorsePicIComponent } from './horse-pic-i/horse-pic-i.component';
import { HorsePicIIComponent } from './horse-pic-ii/horse-pic-ii.component';

@NgModule({
    declarations: [
        AppComponent,
        HorsePicIComponent,
        HorsePicIIComponent
    ],
    imports: [
        BrowserModule,
        AppRoutingModule
    ],
    providers: [],
    bootstrap: [AppComponent]
})
export class AppModule { }
