/**
 * Implement a global theme for components using the context API.  This is in contrast with
 * {@link ThemeWithProps.js}, which uses props.
 * @author Andrew Jarombek
 * @since 1/2/2020
 */

import React from 'react';
import { useHistory } from 'react-router-dom';
import { AJNavTextCircle } from 'jarombek-react-components';

const ThemeWithContext = () => {
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

export default ThemeWithContext;
