// Author: Andrew Jarombek
// Date: 1/6/2018

import { Component } from '@angular/core';

// Create a new Angular component which can be used in HTML with the 'developer' tag
// Angular uses annotations as metadata
@Component({
  selector: 'developer',
  template: '<p>Hello {{dev}}</p>'
})
export class DeveloperComponent {
  dev: string;

  constructor() {
    this.dev = 'Andy';
  }
}
