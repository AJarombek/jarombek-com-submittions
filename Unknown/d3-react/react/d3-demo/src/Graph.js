import React from 'react';
import './Graph.css';
import PropTypes from "prop-types";
import * as d3 from 'd3';
import {withFauxDOM} from 'react-faux-dom';

/**
 * Graph Component
 * @author Andrew Jarombek
 * @since 9/26/2018
 */

class Graph extends React.Component {

    constructor() {
        super();

        // Colors for the bars which are indexed based on the feel property
        this.colors = [
            'rgba(204, 0, 0, .4)',
            'rgba(255, 51, 0, .4)',
            'rgba(204, 102, 0, .4)',
            'rgba(255, 153, 0, .4)',
            'rgba(255, 255, 51, .4)',
            'rgba(187, 187, 187, .4)',
            'rgba(115, 230, 0, .4)',
            'rgba(0, 153, 0, .4)',
            'rgba(0, 102, 0, .4)',
            'rgba(26, 26, 255, .4)'
        ];
    }

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
        const graphHeight = 300;
        const graphPaddingRight = 4;
        const graphData = this.props.data;

        const heightScale = d3.scaleLinear()
            .domain([0, d3.max(graphData, (d) => d.miles)])
            .range([0, graphHeight]);

        const svg = d3.select(faux)
            .append("svg")
            .attr("width", graphWidth)
            .attr("height", graphHeight);

        svg.selectAll("rect")
            .data(graphData)
            .enter()
            .append("rect")
            .attr("x", (d, i) => i * (graphWidth / graphData.length))
            .attr("y", (d) => graphHeight - heightScale(d.miles))
            .attr("width", graphWidth / graphData.length - graphPaddingRight)
            .attr("height", (d) => heightScale(d.miles))
            .attr("fill", (d) => this.colors[d.feel])
            .text((d) => `${d.miles}`);

        svg.selectAll("text")
            .data(graphData)
            .enter()
            .append("text")
            .text((d) => d.miles)
            .attr("x", (d, i) =>
                i * (graphWidth / graphData.length) +
                    (graphWidth / graphData.length - graphPaddingRight) / 2
            )
            .attr("y", (d) => graphHeight - heightScale(d.miles) + 18)
            .attr("class", "graph-label");

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
