/**
 * CreateClass component for Lifecycle events
 * @author Andrew Jarombek
 * @since 1/7/2019
 */

import React from 'react';
import PropTypes from "prop-types";
import createReactClass from 'create-react-class';

// React.createClass() is deprecated as of React 15.5.0.  Instead you can use a separate npm module
// create-react-class
const Lifecycle2 = createReactClass({
    getDefaultProps() {
        return {
            component: 'Unknown'
        };
    },
    getInitialState() {
        return {};
    },
    componentWillMount() {
        if (this.state.willMountInvoked < 3) {
            this.props.updateLifeCycleState("componentWillMount()");
        } else {
            console.info("componentWillMount()");
        }
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
    },
    componentDidMount() {
        this.props.updateLifeCycleState("componentDidMount()");
        console.info("componentDidMount()");
    },
    componentWillUnmount() {
        this.props.updateLifeCycleState("componentWillUnmount()");
        console.info("componentWillUnmount()");
    },
    propTypes: {
        component: PropTypes.string,
        event: PropTypes.string.isRequired,
        parameters: PropTypes.array.isRequired,
        updateLifeCycleState: PropTypes.func.isRequired
    }
});

export default Lifecycle2;