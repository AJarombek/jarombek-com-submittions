/**
 * Main application component for the React 16.3 demo.  Contains a bunch of cards that can be
 * selected to learn more about a React 16.3 feature.
 * @author Andrew Jarombek
 * @since 11/14/2019
 */

import {hot} from 'react-hot-loader/root';
import React from 'react';
import {useHistory} from 'react-router-dom';
import {AJTextCard, AJResponsiveGrid} from 'jarombek-react-components';

const App = () => {
    const history = useHistory();

    return (
        <div className="App">
            <h1>React 16.3</h1>
            <p>Release Date: March 29, 2018</p>
            <AJResponsiveGrid
                smallBreakpoint="500px"
                mediumBreakpoint="900px"
                largeBreakpoint="1200px"
                items={[
                    <AJTextCard
                        key="context"
                        title="Context"
                        content={<>...</>}
                        action={() => history.push('/context')}
                        actionText="Learn More"
                    />,
                    <AJTextCard
                        key="create-ref"
                        title="Create Ref"
                        content={<>...</>}
                        action={() => history.push('/create-ref')}
                        actionText="Learn More"
                    />,
                    <AJTextCard
                        key="forward-ref"
                        title="Forward Ref"
                        content={<>...</>}
                        action={() => history.push('/forward-ref')}
                        actionText="Learn More"
                    />,
                    <AJTextCard
                        key="new-lifecycle"
                        title="New Lifecycle"
                        content={<>...</>}
                        action={() => history.push('/new-lifecycle')}
                        actionText="Learn More"
                    />,
                    <AJTextCard
                        key="strict-mode"
                        title="Strict Mode"
                        content={<>...</>}
                        action={() => history.push('/strict-mode')}
                        actionText="Learn More"
                    />
                ]}
            />
        </div>
    );
};

export default hot(App);
