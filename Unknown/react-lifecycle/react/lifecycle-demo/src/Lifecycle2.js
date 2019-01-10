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
    componentName: "Lifecycle2",
    getDefaultProps() {
        return {
            component: 'Unknown',
            updateLifeCycleState: () => {},
            updateWillMountInvoked: () => {},
            updateDidMountInvoked: () => {},
            updateWillUnmountInvoked: () => {}
        };
    },
    getInitialState() {
        return {};
    },
    componentWillMount() {
        if (this.props.updateWillMountInvoked() < 2) {
            this.props.updateLifeCycleState("componentWillMount()", [], this.componentName);
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
        if (this.props.updateDidMountInvoked() < 2) {
            this.props.updateLifeCycleState("componentDidMount()", [], this.componentName);
        } else {
            console.info("componentDidMount()");
        }
    },
    componentWillUnmount() {
        if (this.props.updateWillUnmountInvoked() < 2) {
            this.props.updateLifeCycleState("componentWillUnmount()", [], this.componentName);
        } else {
            console.info("componentWillUnmount()");
        }
    },
    propTypes: {
        component: PropTypes.string,
        event: PropTypes.string.isRequired,
        parameters: PropTypes.array.isRequired,
        updateLifeCycleState: PropTypes.func,
        updateWillMountInvoked: PropTypes.func,
        updateDidMountInvoked: PropTypes.func,
        updateWillUnmountInvoked: PropTypes.func
    }
});

export default Lifecycle2;