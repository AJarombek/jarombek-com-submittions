/**
 * Example of a component that uses callback refs to focus upon an image.
 * @author Andrew Jarombek
 * @since 1/12/2020
 */

import React from 'react';
import { AJOutlinedButton } from 'jarombek-react-components';

import light from '../assets/light.png';

class ThemeWithContext extends React.Component {

  constructor(props) {
    super(props);

    this.lightFigure = null;

    this.setLightFigureRef = element => this.lightFigure = element;

    this.focusLightFigure = () => {
      if (this.lightFigure) {
        this.lightFigure.focus();
      }
    }
  }

  render () {
    return (
      <div className="callback-ref-sample">
        <figure ref={this.setLightFigureRef}>
          <img src={light} />
        </figure>
        <AJOutlinedButton onClick={this.focusLightFigure} disabled={false}>
          Activate
        </AJOutlinedButton>
      </div>
    );
  }
}

export default ThemeWithContext;
