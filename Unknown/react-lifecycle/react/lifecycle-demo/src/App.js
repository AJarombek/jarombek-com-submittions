/**
 * The applications main component for displaying lifecycles
 * @author Andrew Jarombek
 * @since 1/3/2019
 */

import React, { Component } from 'react';
import './App.css';
import LifecycleList from "./LifecycleList";

class App extends Component {

    static ComponentName = "App";

    /**
     * The constructor is not a lifecycle method, however its the first function invoked after the
     * component is mounted.  The constructor is where the state of the component is initialized.
     * @param props - the properties passed to the component
     */
    constructor(props) {
        super(props);

        // Set the initial state
        this.state = {
            lifeCycles: [
                {
                    component: App.ComponentName,
                    event: "constructor()",
                    parameters: [
                        props
                    ]
                }
            ]
        }
    }

    /**
     * Called when a component is about to mount (the DOM has not rendered yet).  This method is
     * part of the component mounting lifecycle.
     */
    componentWillMount() {
        this.updateLifeCycleState("componentWillMount()");
    }

    /**
     * Called when new properties are passed to the component.  You can call setState() from this
     * component.  This method is part of the component updating lifecycle.
     * @param nextProps - the new properties passed into the component.
     */
    componentWillReceiveProps(nextProps) {
        this.updateLifeCycleState("componentWillReceiveProps()", [nextProps]);
    }

    /**
     * Determine whether the component updates based on state or property changes.  This method is
     * part of the component updating lifecycle.
     * @param nextProps - the new properties passed into the component.
     * @param nextState - the updated state of the component.
     * @return {boolean} true if the component should update, false otherwise.
     */
    shouldComponentUpdate(nextProps, nextState) {
        // setState() can't be invoked from shouldComponentUpdate()
        return true;
    }

    /**
     * Called right before the component updates.  This method is part of the component
     * mounting lifecycle.
     * @param nextProps - the new properties passed into the component.
     * @param nextState - the updated state of the component.
     */
    componentWillUpdate(nextProps, nextState) {
        // setState() can't be invoked from componentWillUpdate()
    }

    /**
     * Render the react element into the DOM.  This method is part of the component
     * mounting lifecycle.
     * @return {*} - a reference to the component
     */
    render() {
        return (
            <div className="App">
                <h1>React Lifecycles</h1>
                <LifecycleList lifecycleList={this.state.life_cycles} />
            </div>
        );
    }

    /**
     * Invoked right after the component renders.  This method is part of the component
     * mounting lifecycle.
     */
    componentDidMount() {
        this.updateLifeCycleState("componentDidMount()");
    }

    /**
     * Invoked right after the component updates.  This method is part of the component
     * updating lifecycle.
     * @param prevProps - the properties of the component prior to the update.
     * @param prevState - the state of the component prior to the update.
     */
    componentDidUpdate(prevProps, prevState) {
        // setState() can't be invoked from componentDidUpdate()
    }

    /**
     * Invoked right before the component is un-mounted (removed from the DOM).  This method is
     * part of the component updating lifecycle.
     */
    componentWillUnmount() {
        this.updateLifeCycleState("componentWillUnmount()");
    }

    /**
     * Add a new lifecycle object to the state.
     * @param event - the lifecycle method that was invoked.
     * @param parameters - the parameters passed to the lifecycle method.
     */
    updateLifeCycleState(event, parameters=[]) {
        const newLifeCycle = {
            component: App.ComponentName,
            event,
            parameters
        };

        this.setState({
            lifeCycles: [
                ...this.state.lifeCycles,
                newLifeCycle
            ]
        });
    }
}

export default App;
