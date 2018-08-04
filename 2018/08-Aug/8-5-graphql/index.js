/**
 * A Basic GraphQL Server
 * @author Andrew Jarombek
 * @since 8/1/2018
 */

const express = require('express');
const graphqlHTTP = require('express-graphql');

const GraphQLSchema = require('./graphQLSchema');
const GraphQLRoot = require('./graphQLRoot');

const app = express();
app.use('/', graphqlHTTP({
    schema: GraphQLSchema,
    rootValue: GraphQLRoot,
    // GraphiQL is a debugging GUI in the browser for GraphQL
    graphiql: true
}));

const port = 4000;
app.listen(port, () => {
    console.info(`GraphQL Server Running on Port ${port}`);
});