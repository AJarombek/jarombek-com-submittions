/**
 * Modularize all the GraphQL Schemas in one file.  The Schemas exist in separate .graphql files
 * @author Andrew Jarombek
 * @since 8/2/2018
 */

const {buildSchema} = require('graphql');
const fs = require('fs');
const path = require('path');

/* The test GraphQL Schema for a Basic Hello World Example */
const testQuery = fs.readFileSync(path.join(__dirname, "query.graphql"), "utf-8");
exports.testSchema = buildSchema(testQuery);

module.exports = exports;