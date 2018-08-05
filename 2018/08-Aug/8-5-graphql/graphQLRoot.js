/**
 * Define the root resolver for the GraphQL schema.
 * @author Andrew Jarombek
 * @since 8/4/2018
 */

const uuid = require('uuid/v4');
const data = require('./exerciseData');

// Each endpoint defined in GraphQL uses a resolver
    // function defined in JavaScript to tell it what to return.
const root = {

    /**
     * Endpoint to simply test that the GraphQL server is up and running
     * @return {string} a welcome message!
     */
    test: () => {
        return 'Hello From GraphQL!';
    },

    /**
     * Get all the exercises in the data store
     * @return {*} an array of exercises
     */
    getAllExercises: () => data.exercises,

    /**
     * Get all the exercises with a given name.  Name is not a unique field.
     * @param arg - arguments passed through the getExercises() field.
     * @return all the exercises that match a name.
     */
    getExercises: (arg) => {
        return data.exercises.filter((exercise) => exercise.name === arg.name);
    },

    /**
     * Get an exercise with a specific ID.  The ID field is unique to an exercise.
     * @param arg - arguments passed through the getExercise() field.
     * @return {null} a single exercise with an ID, null if that ID doesn't match any exercises.
     */
    getExercise: (arg) => {
        const exercise = data.exercises.filter((exercise) => exercise.id === arg.id);
        return exercise.length > 0 ? exercise[0] : null;
    },

    /**
     * Get all exercises from a specific user.  The username is passed to GraphQL
     * to accomplish this.
     * @param arg - arguments passed through the getExercisesByUser() field.
     * @return all the exercises by this user.
     */
    getExercisesByUser: (arg) => {
        return data.exercises.filter((exercise) => exercise.user === arg.user);
    },

    /**
     * Helper method to determine whether an exercise object is a cardio workout
     * @param exercise - an exercise to test whether or not it is a cardio workout
     * @return a truthy value if the argument is a cardio exercise.
     * Otherwise a falsy value is returned.
     */
    isCardioExercise: (exercise) => {
        return exercise.distance || exercise.minutes || exercise.seconds;
    },

    /**
     * Get all the cardio exercises in the data store. NOTE: an arrow function isn't used here
     * to maintain dynamic 'this' and avoid lexical 'this'.  All methods in this object that
     * don't use an arrow function are implemented that way because of this.
     * @return an array of cardio exercises
     */
    getAllCardioExercises: function () {
        return data.exercises.filter((exercise) => this.isCardioExercise(exercise));
    },

    /**
     * Get all the cardio exercises with a given name.  Name is not a unique field.
     * @param arg - arguments passed through the getCardioExercises() field.
     * @return all the cardio exercises that match a name.
     */
    getCardioExercises: function (arg) {
        return data.exercises.filter((exercise) =>
            exercise.name === arg.name && this.isCardioExercise(exercise)
        );
    },

    /**
     * Get a cardio exercise with a specific ID.  The ID field is unique to a cardio exercise.
     * @param arg - arguments passed through the getCardioExercise() field.
     * @return {null} a single cardio exercise with an ID,
     * null if that ID doesn't match any cardio exercises.
     */
    getCardioExercise: function (arg) {
        const cardio = data.exercises.filter((exercise) =>
            exercise.id === arg.id && this.isCardioExercise(exercise)
        );
        return cardio.length > 0 ? cardio[0] : null;
    },

    /**
     * Get all cardio exercises from a specific user.  The username is passed to GraphQL
     * to accomplish this.
     * @param arg - arguments passed through the getCardioExercisesByUser() field.
     * @return all the cardio exercises by this user.
     */
    getCardioExercisesByUser: function (arg) {
        return data.exercises.filter((exercise) =>
            exercise.user === arg.user && this.isCardioExercise(exercise)
        );
    },

    /**
     * Helper method to determine whether an exercise object is a strength workout
     * @param exercise - an exercise on which to test whether or not it is a strength workout
     * @return a truthy value if the argument is a strength exercise.
     * Otherwise a falsy value is returned.
     */
    isStrengthExercise: (exercise) => {
        return exercise.workouts;
    },

    /**
     * Get all the strength exercises in the data store
     * @return an array of strength exercises
     */
    getAllStrengthExercises: function () {
        return data.exercises.filter((exercise) => this.isStrengthExercise(exercise));
    },

    /**
     * Get all the strength exercises with a given name.  Name is not a unique field.
     * @param arg - arguments passed through the getStrengthExercises() field.
     * @return all the strength exercises that match a name.
     */
    getStrengthExercises: function (arg) {
        return data.exercises.filter((exercise) =>
            exercise.name === arg.name && this.isStrengthExercise(exercise)
        );
    },

    /**
     * Get a strength exercise with a specific ID.  The ID field is unique to a strength exercise.
     * @param arg - arguments passed through the getStrengthExercise() field.
     * @return {null} a single strength exercise with an ID,
     * null if that ID doesn't match any strength exercises.
     */
    getStrengthExercise: function (arg) {
        const strength = data.exercises.filter((exercise) =>
            exercise.id === arg.id && this.isStrengthExercise(exercise)
        );
        return strength.length > 0 ? strength[0] : null;
    },

    /**
     * Get all strength exercises from a specific user.  The username is passed to GraphQL
     * to accomplish this.
     * @param arg - arguments passed through the getStrengthExercisesByUser() field.
     * @return all the strength exercises by this user.
     */
    getStrengthExercisesByUser: function (arg) {
        return data.exercises.filter((exercise) =>
            exercise.user === arg.user && this.isStrengthExercise(exercise)
        );
    },

    /**
     * Create a new exercise to be stored as a JavaScript object.  NOTE: This is a helper method,
     * not an entry point specified in GraphQL.
     * @param exercise - an object representing an exercise.
     * @return {{id: string}} the newly created exercise
     */
    createExercise: (exercise) => {
        const newExercise = {id: uuid(), ...exercise};
        data.exercises = [
            ...data.exercises,
            newExercise
        ];
        return newExercise;
    },

    /**
     * Create a new cardio exercise to be stored as a JavaScript object.
     * @param cardio - an object representing a cardio exercise.
     * @return {*|{id: string}} the newly created cardio exercise
     */
    createCardioExercise: function ({cardio}) {
        return this.createExercise(cardio);
    },

    /**
     * Create a new strength exercise to be stored as a JavaScript object.
     * @param strength - an object representing a strength exercise.
     * @return {*|{id: string}} the newly created strength exercise
     */
    createStrengthExercise: function ({strength}) {
        return this.createExercise(strength);
    }
};

module.exports = root;