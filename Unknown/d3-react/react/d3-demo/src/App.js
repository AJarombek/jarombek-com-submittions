import React from 'react';
import Graph from './Graph';
import './App.css';

const data = [
    { value: 2 },
    { value: 4.3 },
    { value: 0 },
    { value: 4.9 },
    { value: 3.3 },
    { value: 7 },
    { value: 5.5 }
];

const App = () =>
    <div className="app">
        <h1>Bar Graph</h1>
        <div className="graph-container">
            <Graph data={data} />
        </div>
    </div>;

export default App;
