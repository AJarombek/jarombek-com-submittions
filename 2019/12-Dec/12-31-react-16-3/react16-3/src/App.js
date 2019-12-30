/**
 * Main application component for the React 16.3 demo.  Contains a bunch of cards that can be
 * selected to learn more about a React 16.3 feature.
 * @author Andrew Jarombek
 * @since 11/14/2019
 */

import React from 'react';
import './App.css';
import {AJTextCard, AJResponsiveGrid} from 'jarombek-react-components';

require('react-dom');
window.React2 = require('react');
console.log(window.React1 === window.React2);

const App = () => {
    return (
        <div className="App">
            <h1>React 16.3</h1>
            <p>Release Date: March 29, 2018</p>
            <AJResponsiveGrid
                smallBreakpoint="400px"
                mediumBreakpoint="600px"
                largeBreakpoint="900px"
                items={[
                    <AJTextCard title={"Context"} />,
                    <AJTextCard title={"Create Ref"} />,
                    <AJTextCard title={"Forward Ref"} />,
                    <AJTextCard title={"New Lifecycle"} />,
                    <AJTextCard title={"Strict Mode"} />
                ]}
            />
        </div>
    );
};

export default App;
