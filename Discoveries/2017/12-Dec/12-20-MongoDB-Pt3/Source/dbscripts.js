// Author: Andrew Jarombek
// Date: 12/20/2017
// MongoDB working with nested objects and arrays

db.user.insert({
    name: "Andrew Jarombek",
    age: 22,
    since: new Date("2011-09-01"),
    logs: [
        {
            date: new Date("2017-12-19"),
            miles: 3.05,
            minutes: 21,
            seconds: 11,
            location: "Riverside CT"
        }
    ],
    plan: [
        {
            date: new Date("2017-12-20"),
            miles: 1
        },
        {
            date: new Date("2017-12-21"),
            miles: 3.05
        },
        {
            date: new Date("2017-12-22"),
            miles: 1
        },
        {
            date: new Date("2017-12-23"),
            miles: 3.05
        },
        {
            date: new Date("2017-12-24"),
            miles: 4.25
        },
        {
            date: new Date("2017-12-25"),
            miles: 1
        },
        {
            date: new Date("2017-12-26"),
            miles: 3.05
        }
    ],
    prs: {
        track: [
            {
                event: "1500m",
                time: "4:08"
            },
            {
                evnt: "5000m",
                time: "15:27"
            }
        ],
        xc: [
            {
                event: "8K",
                time: "26:24"
            }
        ]
    }
})

db.user.findOne()

// You can find nested properties by using the dot notation
db.user.find({'plan.date': new Date("2017-12-25")}, {'plan.date': 1, 'plan.miles': 1}).pretty()

// You can put an index on an array elements (and even a property on an object in an array!)
db.user.createIndex({'plan.date': 1})