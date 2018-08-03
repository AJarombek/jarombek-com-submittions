/**
 * A Basic GraphQL Server
 * @author Andrew Jarombek
 * @since 8/1/2018
 */

const express = require('express');
const graphqlHTTP = require('express-graphql');

const data = require('./dataSource');
const GraphQLSchemas = require('./graphQLSchemas');

class Exercise {

}

const root = {
    test: () => {
        return 'Hello From GraphQL!';
    },
    findExercise: ({name}) => {
        return new Exercise();
    }
};

const app = express();
app.use('/', graphqlHTTP({
    schema: GraphQLSchemas.testSchema,
    rootValue: root,
    graphiql: true
}));

const port = 4000;
app.listen(port, () => {
    console.info(`GraphQL Server Running on Port ${port}`);
});