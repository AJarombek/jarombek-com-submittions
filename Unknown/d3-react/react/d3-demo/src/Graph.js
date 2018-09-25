import React from 'react';
import './Graph.css';
import PropTypes from "prop-types";
import * as d3 from 'd3';

const Graph = ({data}) => {

    d3.select(".graph").append("p").text("Hello From D3");

    return (
        <div className="graph"> </div>
    );
};

Graph.propTypes = {
    data: PropTypes.array.isRequired
};

export default Graph;
