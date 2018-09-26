import React from 'react';
import './Graph.css';
import PropTypes from "prop-types";
import * as d3 from 'd3';
import {withFauxDOM} from 'react-faux-dom';

class Graph extends React.Component {

    static propTypes = {
        data: PropTypes.array.isRequired
    };

    static defaultProps = {
        data: [],
        chart: 'loading'
    };

    componentDidMount() {
        const faux = this.props.connectFauxDOM('div', 'chart');

        const graphWidth = 800;
        const graphHeight = 150;
        const graphPaddingRight = 4;
        const graphData = this.props.data;

        const svg = d3.select(faux)
            .append("svg")
            .attr("width", graphWidth)
            .attr("height", graphHeight);

        svg.selectAll("rect")
            .data(graphData)
            .enter()
            .append("rect")
            .attr("x", (d, i) => i * (graphWidth / graphData.length))
            .attr("y", (d) => graphHeight - (d.value * 10))
            .attr("width", graphWidth / graphData.length - graphPaddingRight)
            .attr("height", (d) => d.value * 10)
            .text((d) => `${d.value}`);

        this.props.animateFauxDOM(800);
    }

    render() {
        return (
            <div className="graph">
                {this.props.chart}
            </div>
        );
    }
}

export default withFauxDOM(Graph);
