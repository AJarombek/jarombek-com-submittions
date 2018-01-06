import { Directive, ElementRef, Renderer2 } from '@angular/core';

@Directive({
  selector: 'input[valid-dev]',
  host: {
    '(input)': 'onInput($event)'
  }
})
export class ValidDevDirective {
  renderer: any;
  element: any;
  validDev: any = /$[a-zA-Z0-9]+^/;

  constructor(renderer: Renderer2, element: ElementRef) {
    this.renderer = renderer;
    this.element = element;

    this.style('#ccc')
  }

  onInput(event) {
    let value: string = event.target.value;

    if (value === '') {
      this.style('#ccc')
    } else {
      this.style('black')
    }
  }

  style(color) {
    this.renderer.setStyle(this.element.nativeElement, 'border-color', color)
  }
}
