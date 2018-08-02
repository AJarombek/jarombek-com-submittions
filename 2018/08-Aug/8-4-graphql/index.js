const {buildSchema} = require('graphql');
const express = require('express');
const graphqlHTTP = require('express-graphql');
const fs = require('fs');
const path = require('path');

const data = require('./dataSource');

/**
 * A Basic GraphQL Server
 * @author Andrew Jarombek
 * @since 8/1/2018
 */

const helloQuery = fs.readFileSync(path.join(__dirname, "test.graphql"), "utf-8");
const schema = buildSchema(helloQuery);

const root = {
    hello: () => {
        return 'Hello World';
    }
};

const app = express();
app.use('/', graphqlHTTP({
    schema,
    rootValue: root,
    graphiql: true
}));

const port = 4000;
app.listen(port, () => {
    console.info(`GraphQL Server Running on Port ${port}`);
});