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
    /**
     * Define the default properties for the component.  In ES6, the default properties are defined
     * in a class field.  This method is part of the component mounting lifecycle.
     * @return {{component: string}} - the default properties
     */
    getDefaultProps() {
        return {
            component: 'Unknown'
        };
    },
    /**
     * Set the initial state of the component.  In ES6, this is performed in the constructor.
     * This method is part of the component mounting lifecycle.
     * @return {{}} - the initial state
     */
    getInitialState() {
        return {};
    },
    /**
     * Called when a component is about to mount (the DOM has not rendered yet).  This method is
     * part of the component mounting lifecycle.
     */
    componentWillMount() {
        console.info("componentWillMount()");
    },
    /**
     * Render the react element into the DOM.  This method is part of the component
     * mounting lifecycle.
     * @return {*} - a reference to the component.
     */
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
    /**
     * Invoked right after the component renders.  This method is part of the component
     * mounting lifecycle.
     */
    componentDidMount() {
        console.info("componentDidMount()");
    },
    /**
     * Invoked right before the component is un-mounted (removed from the DOM).  This method is
     * part of the component updating lifecycle.
     */
    componentWillUnmount() {
        console.info("componentWillUnmount()");
    },
    /**
     * The optional and required properties of the component.
     */
    propTypes: {
        component: PropTypes.string,
        event: PropTypes.string.isRequired,
        parameters: PropTypes.array.isRequired
    }
});

export default Lifecycle2;