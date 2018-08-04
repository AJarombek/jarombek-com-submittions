/**
 * Modularize all the GraphQL Schemas in one file.  The Schemas exist in separate .graphql files
 * @author Andrew Jarombek
 * @since 8/2/2018
 */

const {buildSchema} = require('graphql');
const fs = require('fs');
const path = require('path');

/* Source Files for GraphQL Queries, Mutations, and Types */
const exerciseQL = fs.readFileSync(path.join(__dirname, "exercise.graphql"), "utf-8");
const qlEntryPoint = fs.readFileSync(path.join(__dirname, "query.graphql"), "utf-8");

const ql = `${exerciseQL} ${qlEntryPoint}`;

exports.Schema = buildSchema(ql);

module.exports = exports;