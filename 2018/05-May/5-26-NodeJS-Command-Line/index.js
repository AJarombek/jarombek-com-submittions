#!/usr/bin/env node --harmony

/**
 * Node.JS Command Line Prototype Application - Generate a Random Number
 * @author Andrew Jarombek
 * @since 5/26/2018
 */

const program = require('commander');

const helpMessage = `
  Examples:
    "Return a random floating point number between 0 and 1"
      random 0 1 -t float 
        
    "Return a random integer number between 1 and 10"
      random 1 10 -t int
`;

program
    .version('1.0')
    .arguments('<start> <end>')
    .option('-t, --type <type>', 'Type of Number')
    .action((start, end) => {
        console.log(`Start: ${start}`);
        console.log(`End: ${end}`);
        console.log(`Type: ${program.type}`);

        const range = end - start;

        const random = Math.random();
        const randomInRange = random * range;
        const randomInRangeFromStart = randomInRange + start;

        if (program.type && (program.type.toLowerCase() === 'int' ||
            program.type.toLowerCase() === 'integer')) {

            console.log(Math.round(randomInRangeFromStart));
        } else {
            console.log(randomInRangeFromStart);
        }

    })
    .on('--help', () => {
        console.log(helpMessage)
    })
    .parse(process.argv);