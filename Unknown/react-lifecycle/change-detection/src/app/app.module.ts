/**
 * Configure the module for the application
 * @author Andrew Jarombek
 * @since 1/17/2019
 */

import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';

import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { CatPicIComponent } from './cat-pic-i/cat-pic-i.component';
import { CatPicIIComponent } from './cat-pic-ii/cat-pic-ii.component';

@NgModule({
    declarations: [
        AppComponent,
        CatPicIComponent,
        CatPicIIComponent
    ],
    imports: [
        BrowserModule,
        AppRoutingModule
    ],
    providers: [],
    bootstrap: [AppComponent]
})
export class AppModule { }
