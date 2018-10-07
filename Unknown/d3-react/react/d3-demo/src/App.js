import React from 'react';
import Graph from './Graph';
import './App.css';
import runData from './runData';

/**
 * App Component
 * @author Andrew Jarombek
 * @since 9/26/2018
 */

class App extends React.Component {

    constructor() {
        super();

        // The running logs initially shown in the graph are from the latest week
        this.state = {
            dataIndex: runData.length - 1
        };
    }

    /**
     * View the previous week of running logs
     */
    onClickPrev() {
        console.info(`Click Previous to index: ${this.state.dataIndex - 1}`);
        this.setState({dataIndex: this.state.dataIndex - 1});
    }

    /**
     * View the next week of running logs
     */
    onClickNext() {
        console.info(`Click Next to index: ${this.state.dataIndex + 1}`);
        this.setState({dataIndex: this.state.dataIndex + 1});
    }

    render() {
        return (
            <div className="app">
                <h1>Bar Graph</h1>
                <button onClick={() => this.onClickPrev()}>Prev</button>
                <button onClick={() => this.onClickNext()}>Next</button>
                <div className="graph-container">
                    <Graph data={runData[this.state.dataIndex]} />
                </div>
            </div>
        );
    }
}

export default App;
