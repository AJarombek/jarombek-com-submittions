/**
 * Modularize all the GraphQL Schemas in one file.  The Schemas exist in separate .graphql files
 * @author Andrew Jarombek
 * @since 8/2/2018
 */

const {GraphQLScalarType} = require('graphql');
const {Kind} = require('graphql/language');
const {makeExecutableSchema} = require('graphql-tools');
const fs = require('fs');
const path = require('path');

// Source Files for GraphQL Queries, Mutations, and Types
const exerciseTypes = fs.readFileSync(path.join(__dirname, "exercise.graphql"), "utf-8");
const entryPoint = fs.readFileSync(path.join(__dirname, "entrypoints.graphql"), "utf-8");

// Build a full GraphQL schema out of all the source .graphql files
const graphQLSchema = `${exerciseTypes} ${entryPoint}`;

// Define a scalar type (a leaf type of a query - think primitive type) for the GraphQL schema
// This type represents a Date - since GraphQL does not have a Date scalar type
const DateScalar = new GraphQLScalarType({
    name: 'Date',
    description: 'Scalar type representing a date in time',
    // Parse an external value sent to GraphQL as a Date type.
    // ParseValue() is called when an input value is sent to GraphQL from
    // the client through variables.
    parseValue(value) {
        return Date.parse(value)
    },
    // Serialize an internally stored value in GraphQL to return as a response
    // In this case, GraphQL will return the date in milliseconds
    serialize(value) {
        return value.getTime()
    },
    // Parse an external value sent to GraphQL as a Date type.  This function handles incoming
    // values of type ValueNode (which corresponds to all JSON types).
    // ParseLiteral() is called when input values are sent to GraphQL through an inline query
    parseLiteral(valueNode) {
        if (valueNode.kind === Kind.INT || valueNode.kind === Kind.STRING) {
            return new Date(valueNode.value);
        }
        return null;
    }
});

const ExerciseTypes = {
    Exercise: {
        __resolveType(obj, context, info) {
            if (obj.distance || obj.minutes) {
                return 'Cardio'
            } else {
                return 'Strength'
            }
        }
    }
};

const resolvers = {
    Date: DateScalar,
    Exercise: ExerciseTypes.Exercise
};

const Schema = makeExecutableSchema({
    typeDefs: graphQLSchema,
    resolvers,
    resolverValidationOptions: {
        requireResolversForResolveType: false
    }
});

module.exports = Schema;