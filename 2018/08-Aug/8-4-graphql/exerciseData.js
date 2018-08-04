const ExerciseData = {
    exercises: [
        {
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