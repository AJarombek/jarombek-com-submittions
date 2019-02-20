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

const LifecycleList = ({ lifecycleList=[], version=1 }) =>
    <div className="lifecycle-list">
        {(lifecycleList.length === 0) ?
            <p>No Data</p> :
            lifecycleList.map(lifecycle =>
                { return (version === 1) ?
                    <Lifecycle key={uuid()} {...lifecycle} />:
                    <Lifecycle2 key={uuid()} {...lifecycle} />;
                }
            )
        }
    </div>;

LifecycleList.propTypes = {
    lifecycleList: PropTypes.array,
    version: PropTypes.number
};

export default LifecycleList;