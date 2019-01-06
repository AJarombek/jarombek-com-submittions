/**
 * Functional component for Lifecycle events
 * @author Andrew Jarombek
 * @since 1/6/2019
 */

import React from 'react';
import PropTypes from 'prop-types';

const Lifecycle = ({ component, event, parameters }) =>
    <div className="lifecycle">
        <p className="lifecycle-component">{component}</p>
        <p className="lifecycle-event">{event}</p>
        <p className="lifecycle-parameters">{parameters}</p>
    </div>;

Lifecycle.propTypes = {
    list: PropTypes.string.isRequired,
    event: PropTypes.string.isRequired,
    parameters: PropTypes.array.isRequired
};

export default Lifecycle;