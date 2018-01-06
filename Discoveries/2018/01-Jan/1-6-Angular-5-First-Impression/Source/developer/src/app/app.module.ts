// Author: Andrew Jarombek
// Date: 1/6/2018

import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';

import { DeveloperComponent } from './app.component';

// Wrap our component into a module.  Each file can be one module which can be exported.
@NgModule({
  declarations: [DeveloperComponent],
  imports: [BrowserModule],
  providers: [],
  bootstrap: [DeveloperComponent]
})
export class AppModule { }
