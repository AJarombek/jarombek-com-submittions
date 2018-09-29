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
            'rgb(153, 0, 0)',
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
        console.info('Inside Graph componentDidMount');
        this.generateGraph(this.props);
    }

    componentWillReceiveProps(nextProps) {
        console.info('Inside Graph componentWillReceiveProps');
        // this.generateGraph(nextProps);
    }

    generateGraph(props) {
        const faux = props.connectFauxDOM('div', 'chart');

        // Measurements for the graph
        const graphWidth = 800;
        const graphHeight = 300;
        const graphPaddingBottom = 30;

        // The data displayed in the graph
        const graphData = props.data;

        // A D3 Scale for the height of each bar in the graph
        const heightScale = d3.scaleLinear()
            .domain([0, d3.max(graphData, (d) => d.miles)])
            .range([10, graphHeight]);

        // A D3 Scale for the width of each bar in the graph
        const widthScale = d3.scaleBand()
            .domain(d3.range(graphData.length))
            .rangeRound([0, graphWidth])
            .paddingInner(0.05);

        // A D3 Scale for the width of the x-axis of the graph
        const xAxisScale = d3.scaleTime()
            .domain([
                d3.min(graphData, (d) => d.date),
                d3.max(graphData, (d) => d.date)
            ])
            .range([
                (graphWidth / graphData.length) / 2,
                graphWidth - (graphWidth / graphData.length) / 2
            ]);

        // A format for the date displayed on each x-axis tick mark
        const dayOfWeek = d3.timeFormat("%a, %b. %d");

        // The x-axis displayed under the graph
        const xAxis = d3.axisBottom()
            .scale(xAxisScale)
            .ticks(7)
            .tickFormat(dayOfWeek);

        // The svg container for the graph
        const svg = d3.select(faux)
            .append("svg")
            .attr("width", graphWidth)
            .attr("height", graphHeight + graphPaddingBottom);

        // For each data point, create a rectangle bar in the graph
        svg.selectAll("rect")
            .data(graphData)
            .enter()
            .append("rect")
            .attr("x", (d, i) => widthScale(i))
            .attr("y", (d) => graphHeight - heightScale(d.miles))
            .attr("width", widthScale.bandwidth())
            .attr("height", (d) => heightScale(d.miles))
            .attr("fill", (d) => this.colors[d.feel])
            .text((d) => `${d.miles}`);

        // For each data point, create a label for each bar.
        // The label displays the 'miles' property.
        svg.selectAll("text")
            .data(graphData)
            .enter()
            .append("text")
            .text((d) => d.miles)
            .attr("x", (d, i) => widthScale(i) + widthScale.bandwidth() / 2)
            .attr("y", (d) => graphHeight - heightScale(d.miles) + 18)
            .attr("class", "graph-label");

        // Add the x-axis to the bottom of the graph
        svg.append("g")
            .attr("class", "x-axis")
            .attr("transform", `translate(0,${graphHeight + 5})`)
            .call(xAxis);

        props.animateFauxDOM(800);
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
