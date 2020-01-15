/**
 * Describe the forwardRef API introduced in React 16.3.
 * @author Andrew Jarombek
 * @since 1/15/2020
 */

import React from 'react';
import FeaturePage from './FeaturePage';
import ForwardRefSample from './forwardref/ForwardRefSample';

const ForwardRefDemo = () => {
  return (
    <FeaturePage>
      <h1>ForwardRef API</h1>
      <div className="demo-body">
        <p>
          ...
        </p>
        <ForwardRefSample />
      </div>
    </FeaturePage>
  );
};

export default ForwardRefDemo;
