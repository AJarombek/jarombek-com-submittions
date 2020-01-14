/**
 * Describe the createRef API introduced in React 16.3.
 * @author Andrew Jarombek
 * @since 1/12/2020
 */

import React from 'react';
import FeaturePage from './FeaturePage';
import RefSamples from './createref/RefSamples';

const CreateRefDemo = () => {
    return (
        <FeaturePage>
            <h1>CreateRef API</h1>
          <div className="demo-body">
            <p>
              Text
            </p>
            <RefSamples />
          </div>
        </FeaturePage>
    );
};

export default CreateRefDemo;
