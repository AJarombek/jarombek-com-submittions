/**
 * Functional component for Lifecycle events
 * @author Andrew Jarombek
 * @since 1/6/2019
 */

import React from 'react';
import PropTypes from 'prop-types';
import './Lifecycle.css';

const Lifecycle = ({ component, event, parameters }) =>
    <div className="lifecycle">
        <p className="lifecycle-component">{component}</p>
        <p className="lifecycle-event">{event}</p>
        <p className="lifecycle-parameters">{JSON.stringify(parameters)}</p>
    </div>;

Lifecycle.propTypes = {
    component: PropTypes.string.isRequired,
    event: PropTypes.string.isRequired,
    parameters: PropTypes.array.isRequired
};

export default Lifecycle;