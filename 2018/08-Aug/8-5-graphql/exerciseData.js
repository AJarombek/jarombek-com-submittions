/**
 * Data store for GraphQL - in the future this primitive store can be replaced
 * with a full fledged MongoDB database
 * @author Andrew Jarombek
 * @since 8/4/2018
 */

const uuid = require('uuid/v4');

const ExerciseData = {
    exercises: [
        {
            id: uuid(),
            name: "Early Morning Beach Run",
            user: "Andy",
            description: "Morning run on the beach with a few strides",
            date: Date.parse("Aug 3, 2018"),
            distance: 3.12,
            minutes: 23,
            seconds: 46,
            type: "RUN"
        },
        {
            id: uuid(),
            name: "Track Workout at High School",
            user: "Andy",
            description: `Was drizzling during the warmup and raining for the workout.  
                6x1000m at a much slower pace than in college (3:12-3:20)`,
            date: Date.parse("Aug 4, 2018"),
            distance: 8.75,
            type: "RUN"
        },
        {
            id: uuid(),
            name: "Push-Ups at Home",
            user: "Andy",
            description: "It was exhausting",
            date: Date.parse("Aug 4, 2018"),
            workouts: ["Push-Ups", "Sit-Ups"],
            type: "CORE"
        }
    ]
};

module.exports = ExerciseData;