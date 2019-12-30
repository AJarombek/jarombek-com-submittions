/**
 * Main application component for the React 16.3 demo.  Contains a bunch of cards that can be
 * selected to learn more about a React 16.3 feature.
 * @author Andrew Jarombek
 * @since 11/14/2019
 */

import {hot} from 'react-hot-loader/root';
import React from 'react';
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
                    <AJTextCard
                        key="content"
                        title="Context"
                        content={<>...</>}
                        action={f=>f}
                        actionText="Learn More"
                    />,
                    <AJTextCard
                        key="create-ref"
                        title="Create Ref"
                        content={<>...</>}
                        action={f=>f}
                        actionText="Learn More"
                    />,
                    <AJTextCard
                        key="forward-ref"
                        title="Forward Ref"
                        content={<>...</>}
                        action={f=>f}
                        actionText="Learn More"
                    />,
                    <AJTextCard
                        key="new-lifecycle"
                        title="New Lifecycle"
                        content={<>...</>}
                        action={f=>f}
                        actionText="Learn More"
                    />,
                    <AJTextCard
                        key="strict-mode"
                        title="Strict Mode"
                        content={<>...</>}
                        action={f=>f}
                        actionText="Learn More"
                    />
                ]}
            />
        </div>
    );
};

export default hot(App);
