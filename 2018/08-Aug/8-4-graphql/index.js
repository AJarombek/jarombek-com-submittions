/**
 * A Basic GraphQL Server
 * @author Andrew Jarombek
 * @since 8/1/2018
 */

const express = require('express');
const graphqlHTTP = require('express-graphql');

const data = require('./exerciseData');
const GraphQLSchema = require('./graphQLSchema');

const root = {
    test: () => {
        return 'Hello From GraphQL!';
    },
    getExercises: (arg) => {
        return data.exercises.filter((exercise) => exercise.name === arg.name);
    },
    getExercise: (arg) => {
        const exercise = data.exercises.filter((exercise) => exercise.id === arg.id);
        return exercise.length > 0 ? exercise[0] : null;
    },
    getExercisesByUser: (arg) => {
        console.info(arg);
        const retVal =  data.exercises.filter((exercise) => exercise.user === arg.user);
        console.info(retVal);
        return retVal;
    },
    createExercise: (exercise) => {
        const newExercise = {id: "ID", ...newExercise};
        data.exercises = [
            ...data.exercises,
            newExercise
        ];
        return newExercise;
    },
    createCardioExercise: (cardioExercise) => {
        return this.createExercise(cardioExercise);
    },
    createStrengthExercise: (strengthExercise) => {
        return this.createExercise(strengthExercise);
    }
};

const app = express();
app.use('/', graphqlHTTP({
    schema: GraphQLSchema,
    rootValue: root,
    graphiql: true
}));

const port = 4000;
app.listen(port, () => {
    console.info(`GraphQL Server Running on Port ${port}`);
});