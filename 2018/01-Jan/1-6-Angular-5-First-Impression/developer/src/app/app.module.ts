// Author: Andrew Jarombek
// Date: 1/6/2018

import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';

import { DeveloperComponent } from './app.component';
import { ValidDevDirective } from './valid-dev.directive';

// Wrap our component and directive into a module.  Each file can be one module which can be exported.
// You must import BrowserModule in the root module
@NgModule({
  imports: [BrowserModule],
  declarations: [DeveloperComponent, ValidDevDirective],
  bootstrap: [DeveloperComponent]
})
export class AppModule { }
