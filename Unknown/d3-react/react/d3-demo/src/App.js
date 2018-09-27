import React from 'react';
import Graph from './Graph';
import './App.css';

/**
 * App Component
 * @author Andrew Jarombek
 * @since 9/26/2018
 */

// Data to pass to the D3 Graph
const data = [
    {
        miles: 2,
        feel: 5
    },
    {
        miles: 4.3,
        feel: 7
    },
    {
        miles: 1,
        feel: 3
    },
    {
        miles: 4.9,
        feel: 2
    },
    {
        miles: 3.3,
        feel: 8
    },
    {
        miles: 7,
        feel: 5
    },
    {
        miles: 5.5,
        feel: 6
    }
];

const App = () =>
    <div className="app">
        <h1>Bar Graph</h1>
        <div className="graph-container">
            <Graph data={data} />
        </div>
    </div>;

export default App;
