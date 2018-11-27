import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';
import { AppComponent } from './app.component';
import {RouterModule, Routes} from "@angular/router";
import { HomeComponent } from './home/home.component';
import { NotificationComponent } from './notification/notification.component';
import {LifecycleService} from "./lifecycle.service";

/**
 * Module for the Lifecycle Demo Application
 * @author Andrew Jarombek
 * @since 9/17/2018
 */

export const routes: Routes = [
    {path: '', component: HomeComponent},
    {path: '**', redirectTo: ''}
];

@NgModule({
    declarations: [
        AppComponent,
        HomeComponent,
        NotificationComponent
    ],
    imports: [
        BrowserModule,
        RouterModule.forRoot(routes)
    ],
    providers: [
        LifecycleService
    ],
    bootstrap: [AppComponent]
})
export class AppModule { }
