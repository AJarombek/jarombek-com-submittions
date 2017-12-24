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
db.user.find({'plan.date': new Date("2017-12-25")}, {'plan.miles': 1}).pretty()

// You can put an index on array elements (and even a property of an object in an array!)
db.user.createIndex({'plan.date': 1})

db.user.find({}, {_id: 1, plan: {$slice: 1}}).pretty()

db.user.find({name: "Andrew Jarombek"}, {'prs.track': 1})

// Delete the first element of the plan array
db.user.update({}, {$pop: {'plan': -1}})

// Add an element to the array if it does not already exist
db.user.update({}, {$addToSet: {'plan': {date: new Date("2017-12-27"), miles: 2}}})

db.user.findAndModify({
    query: {},
    update: {
        $addToSet: {'plan': {date: new Date("2017-12-28"), miles: 4.1}}
    },
    'new': true
})