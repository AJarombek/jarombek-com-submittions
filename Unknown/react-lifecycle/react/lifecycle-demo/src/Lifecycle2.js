/**
 * CreateClass component for Lifecycle events
 * @author Andrew Jarombek
 * @since 1/7/2019
 */

import React from 'react';
import PropTypes from "prop-types";

const Lifecycle2 = React.createClass({
    propTypes: {
        component: PropTypes.string.isRequired,
        event: PropTypes.string.isRequired,
        parameters: PropTypes.array.isRequired
    },
    render() {
        const { component, event, parameters } = this.props;
        return (
            <div className="lifecycle">
                <p className="lifecycle-component">{component}</p>
                <p className="lifecycle-event">{event}</p>
                <p className="lifecycle-parameters">{JSON.stringify(parameters)}</p>
            </div>
        );
    }
});

export default Lifecycle2;