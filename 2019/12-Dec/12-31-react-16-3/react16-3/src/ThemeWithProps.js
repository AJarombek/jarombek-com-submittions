/**
 * Implement a global theme for components using props.  This is in contrast with
 * {@link ThemeWithContext.js}, which uses the context API.
 * @author Andrew Jarombek
 * @since 1/2/2020
 */

import React from 'react';
import { useHistory } from 'react-router-dom';
import { AJNavTextCircle } from 'jarombek-react-components';
import {AJTextButton, AJSwitchIcon} from 'jarombek-react-components';

const ThemeWithProps = () => {
  return (
    <div>
      <AJSwitchIcon
        initialState={true}
        disabled={false}
        onChange={() => {}}
        offImageUrl="assets/green.png"
        onImageUrl="assets/light.png"
      />
    </div>
  );
};

export default ThemeWithProps;
