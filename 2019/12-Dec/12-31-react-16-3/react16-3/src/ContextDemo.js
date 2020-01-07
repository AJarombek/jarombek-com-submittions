/**
 * Describe the context API introduced in React 16.3.
 * @author Andrew Jarombek
 * @since 12/30/2019
 */

import React from 'react';
import { useHistory } from 'react-router-dom';
import { AJNavTextCircle } from 'jarombek-react-components';
import FeaturePage from './FeaturePage';
import ThemeWithProps from './ThemeWithProps';
import ThemeWithContext from './ThemeWithContext';

const ContextDemo = () => {
  return (
    <FeaturePage>
      <h1>Context API</h1>
      <div className="context-demo-body">
        <p>
          In React 16.3, the Context API was rewritten. Context allows data to be shared
          amongst components without passing it via props from parents to children<sup>1</sup>.
          While I'm still learning which scenarios are appropriate for the context API, the React
          blog recommends to use it for "global" data used by a tree of React
          components<sup>2</sup>.
        </p>
        <p>
          One good use case for the context API is setting the stylistic theme of a web page based
          on a toggleable switch.  All the components on a page should be aware of the theme chosen,
          but ideally it doesn't need to be passed to every component via a prop.
        </p>
        <p>
          First I implemented this scenario using props.
        </p>
        <ThemeWithProps />
        <p>
          Next I implemented the same component with the context API.
        </p>
        <ThemeWithContext />
      </div>
    </FeaturePage>
  );
};

export default ContextDemo;
