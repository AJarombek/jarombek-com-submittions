/**
 * Functional component holding a list of lifecycle components
 * @author Andrew Jarombek
 * @since 1/6/2019
 */

import React from 'react';
import PropTypes from 'prop-types';
import Lifecycle from './Lifecycle';
import Lifecycle2 from './Lifecycle2';
import uuid from 'uuid/v4';

const LifecycleList = ({ lifecycleList=[], version=1, updateLifeCycleState=() => {},
                           updateWillMountInvoked=() => {}, updateDidMountInvoked=() => {},
                           updateWillUnmountInvoked=() => {} }) =>
    <div className="lifecycle-list">
        {(lifecycleList.length === 0) ?
            <p>No Data</p> :
            lifecycleList.map(lifecycle =>
                { return (version === 1) ?
                    <Lifecycle key={uuid()} {...lifecycle} />:
                    <Lifecycle2 key={uuid()} {...lifecycle}
                                updateLifeCycleState={updateLifeCycleState}
                                updateWillMountInvoked={updateWillMountInvoked}
                                updateDidMountInvoked={updateDidMountInvoked}
                                updateWillUnmountInvoked={updateWillUnmountInvoked} />;
                }
            )
        }
    </div>;

LifecycleList.propTypes = {
    lifecycleList: PropTypes.array,
    version: PropTypes.number,
    // These are only needed for Lifecycle2
    updateLifeCycleState: PropTypes.func,
    updateWillMountInvoked: PropTypes.func,
    updateDidMountInvoked: PropTypes.func,
    updateWillUnmountInvoked: PropTypes.func
};

export default LifecycleList;