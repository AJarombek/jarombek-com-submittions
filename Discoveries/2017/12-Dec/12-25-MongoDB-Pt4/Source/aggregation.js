// Author: Andrew Jarombek
// Date: 12/24/2017
// MongoDB Aggregation Framework Shell commands for the christmas tree database

// Get the number of trees available for each grade sorted by the tree count (frazier only)
db.tree.aggregate([
    {$match: {type: 'frazier'}},
    {$group: {
        _id: '$grade', 
        count: {$sum:1}
    }},
    {$sort: {count: -1}}
])

// Same query execpt for all trees - notice that no match pipeline step is necessary
db.tree.aggregate([
    {$group: {
        _id: {type: '$type', grade: '$grade'}, 
        count: {$sum:1}
    }},
    {$sort: {count: -1}}
])

// For each type and grade, get the tree count, expenses, revenue, and profit
// Then put the results into a collection called 'treestat'
db.tree.aggregate([
    {$group: {
        _id: {
            type: '$type', 
            grade: '$grade', 
            source: '$source_price', 
            sell: '$sell_price'
        }, 
        count: {$sum:1},
        expenses: {$sum: '$source_price'},
        revenue: {$sum: '$sell_price'}
    }},
    {$group: {
        _id: {
            type: '$_id.type', 
            grade: '$_id.grade', 
            source: '$_id.source', 
            sell: '$_id.sell', 
            count: '$count', 
            expenses: '$expenses', 
            revenue: '$revenue'
        },
        profit: {$sum: {$subtract: ['$revenue', '$expenses']}}
    }},
    {$sort: {profit: -1}},
    {$project: {
        _id: {
            type: '$_id.type', 
            grade: '$_id.grade'
        },
        count: '$_id.count', 
        expenses: '$_id.expenses', 
        revenue: '$_id.revenue',
        profit: '$profit'
    }},
    {$out: 'treestat'}
])

db.treestat.find().pretty()

// Get the statistics for all the trees
db.treestat.aggregate([
    {$group: {
        _id: 'all_trees',
        total_trees: {$sum: '$count'},
        total_expenses: {$sum: '$expenses'},
        total_revenue: {$sum: '$revenue'},
        total_profit: {$sum: '$profit'}
    }}
])