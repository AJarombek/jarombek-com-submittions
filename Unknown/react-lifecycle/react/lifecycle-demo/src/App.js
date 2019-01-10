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
            // The version of the Lifecycle component currently in use. version in {1,2}
            version: 1,
            // Incremented each time the "Update" button is clicked
            updateCount: 0,
            // Incremented each time the "Shouldn't Update" button is clicked
            noUpdateCount: 0,
            // Was componentWillMount() invoked in App
            willMountInvoked: false,
            // Was componentWillReceiveProps() invoked in App
            willReceivePropsInvoked: false,
            // Was componentDidMount() invoked in App
            didMountInvoked: false,
            // Was componentWillUnmount() invoked in App
            willUnmountInvoked: false,
            // Incremented each time componentWillMount() is invoked in the Lifecycle2 component
            lifecycleWillMountCount: 0,
            // Incremented each time componentDidMount() is invoked in the Lifecycle2 component
            lifecycleDidMountCount: 0,
            // Incremented each time componentWillUnmount() is invoked in the Lifecycle2 component
            lifecycleWillUnmountCount: 0,
            // Array of objects passed to the Lifecycle & Lifecycle2 components
            lifeCycles: [
                {
                    component: App.ComponentName,
                    event: "constructor()",
                    parameters: [
                        props
                    ]
                }
            ]
        };
    }

    /**
     * Called when a component is about to mount (the DOM has not rendered yet).  This method is
     * part of the component mounting lifecycle.
     */
    componentWillMount() {
        if (!this.state.willMountInvoked) {
            this.updateLifeCycleState("componentWillMount()");
            this.setState({willMountInvoked: true});
        }
    }

    /**
     * Called when new properties are passed to the component.  You can call setState() from this
     * component.  This method is part of the component updating lifecycle.
     * @param nextProps - the new properties passed into the component.
     */
    componentWillReceiveProps(nextProps) {
        if (!this.state.willReceivePropsInvoked) {
            this.updateLifeCycleState("componentWillReceiveProps()", [nextProps]);
            this.setState({willReceivePropsInvoked: true});
        }
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
        console.info("shouldComponentUpdate()");
        return this.state.noUpdateCount === nextState.noUpdateCount;
    }

    /**
     * Called right before the component updates.  This method is part of the component
     * mounting lifecycle.
     * @param nextProps - the new properties passed into the component.
     * @param nextState - the updated state of the component.
     */
    componentWillUpdate(nextProps, nextState) {
        // setState() can't be invoked from componentWillUpdate()
        console.info("componentWillUpdate()");
    }

    /**
     * Render the react element into the DOM.  This method is part of the component
     * mounting lifecycle.
     * @return {*} - a reference to the component
     */
    render() {
        const {lifeCycles, version} = this.state;
        return (
            <div className="App">
                <h1>React Lifecycles (Version {version})</h1>
                <div className="buttons">
                    <button className="btn1" onClick={() => this.update()}>
                        Update
                    </button>
                    <button className="btn2" onClick={() => this.noUpdate()}>
                        Shouldn't Update
                    </button>
                    <button className="btn1" onClick={() => this.updateVersion()}>
                        Switch to Version {this.state.version === 1 ? 2 : 1}
                    </button>
                </div>
                {/* bind() creates an immutable bind of 'this' to 'updateLifeCycleState' */}
                {/* for all future invocations. */}
                <LifecycleList lifecycleList={lifeCycles} version={version}
                               updateLifeCycleState={this.updateLifeCycleState.bind(this)}
                               updateWillMountInvoked={this.updateWillMountInvoked.bind(this)}
                               updateDidMountInvoked={this.updateDidMountInvoked.bind(this)}
                               updateWillUnmountInvoked={this.updateWillUnmountInvoked.bind(this)}
                />
            </div>
        );
    }

    /**
     * Invoked right after the component renders.  This method is part of the component
     * mounting lifecycle.
     */
    componentDidMount() {
        if (!this.state.didMountInvoked) {
            this.updateLifeCycleState("componentDidMount()");
            this.setState({didMountInvoked: true});
        }
    }

    /**
     * Invoked right after the component updates.  This method is part of the component
     * updating lifecycle.
     * @param prevProps - the properties of the component prior to the update.
     * @param prevState - the state of the component prior to the update.
     */
    componentDidUpdate(prevProps, prevState) {
        // setState() can't be invoked from componentDidUpdate()
        console.info("componentDidUpdate()");
    }

    /**
     * Invoked right before the component is un-mounted (removed from the DOM).  This method is
     * part of the component updating lifecycle.
     */
    componentWillUnmount() {
        if (!this.state.willUnmountInvoked) {
            this.updateLifeCycleState("componentWillUnmount()");
            this.setState({willUnmountInvoked: true});
        }
    }

    /**
     * Add a new lifecycle object to the state.
     * @param event - the lifecycle method that was invoked.
     * @param parameters - the parameters passed to the lifecycle method.
     * @param component - the name of the component that had a lifecycle event.
     */
    updateLifeCycleState(event, parameters=[], component=App.ComponentName) {
        const newLifeCycle = {
            component,
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

    /**
     * Update the lifecycle component version that is displayed.
     */
    updateVersion() {
        const version = this.state.version === 1 ? 2 : 1;
        this.setState({version});
    }

    /**
     * Change the component state in a way that will update the component.
     */
    update() {
        this.setState({
            updateCount: this.state.updateCount + 1
        });
    }

    /**
     * Change the component state in a way that won't update the component.
     */
    noUpdate() {
        this.setState({
            noUpdateCount: this.state.noUpdateCount + 1
        });
    }

    /**
     * Update the number of times componentWillMount() is invoked in the Lifecycle2 component
     */
    updateWillMountInvoked() {
        const updatedInvokedCount = this.state.lifecycleWillMountCount + 1;
        if (updatedInvokedCount < 3) {
            this.setState({
                lifecycleWillMountCount: updatedInvokedCount
            });
        }

        return updatedInvokedCount;
    }

    /**
     * Update the number of times componentDidMount() is invoked in the Lifecycle2 component
     */
    updateDidMountInvoked() {
        const updatedInvokedCount = this.state.lifecycleDidMountCount + 1;
        if (updatedInvokedCount < 3) {
            this.setState({
                lifecycleDidMountCount: updatedInvokedCount
            });
        }

        return updatedInvokedCount;
    }

    /**
     * Update the number of times componentWillUnmount() is invoked in the Lifecycle2 component
     */
    updateWillUnmountInvoked() {
        const updatedInvokedCount = this.state.lifecycleWillUnmountCount + 1;
        if (updatedInvokedCount < 3) {
            this.setState({
                lifecycleWillUnmountCount: updatedInvokedCount
            });
        }

        return updatedInvokedCount;
    }
}

export default App;
