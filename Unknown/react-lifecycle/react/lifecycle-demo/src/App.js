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

    componentWillReceiveProps(nextProps) {
        this.updateLifeCycleState("componentWillReceiveProps()");
    }

    shouldComponentUpdate(nextProps, nextState) {
        this.updateLifeCycleState("shouldComponentUpdate()");
    }

    componentWillUpdate(nextProps, nextState) {
        this.updateLifeCycleState("componentWillUpdate()");
    }

    /**
     * Render the react element into the DOM.  This method is part of the component
     * mounting lifecycle.
     * @return {*} - a reference to the component
     */
    render() {
        return (
            <div className="App">
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

    componentDidUpdate(prevProps, prevState) {
        this.updateLifeCycleState("componentDidUpdate()");
    }

    componentWillUnmount() {
        this.updateLifeCycleState("componentWillUnmount()");
    }

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
