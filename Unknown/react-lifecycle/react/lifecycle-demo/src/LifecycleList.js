/**
 * Functional component holding a list of lifecycle components
 * @author Andrew Jarombek
 * @since 1/6/2019
 */

import React from 'react';
import PropTypes from 'prop-types';
import Lifecycle from './Lifecycle';
import uuid from 'uuid/v4';

const LifecycleList = ({ lifecycleList=[] }) =>
    <div className="lifecycle-list">
        { (lifecycleList.length === 0) ?
            <p>No Data</p> :
            lifecycleList.map(lifecycle =>
                <Lifecycle key={uuid()} {...lifecycle} />
            )
        }
    </div>;

LifecycleList.propTypes = {
    lifecycleList: PropTypes.array
};

export default LifecycleList;