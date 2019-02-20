# Running our ES6 code will not work if the environment is not ES6 compliant
#import * as ts from 'taylorSwift';
#^^^^^^
#SyntaxError: Unexpected token import
node main.js

# Create package.json for babel dependencies
npm init

# Install babel
npm install --save-dev babel-cli

npm install babel-preset-env --save-dev

# Still won't work - we need to specify which files to compile for babel'
node main.js

# npx will make our babel commands easier
npm install -g npx

# print the compiled pre-ES6 code to stdout
npx babel main.js

# Compile to an output file
npx babel main.js --out-file es5/main.js

# Also you can watch the input file so the output updates on changes
# I ran these in the background so the bash shell would not be blocked
npx babel main.js --watch --out-file es5/main.js &
npx babel taylorSwift.js --watch --out-file es5/taylorSwift.js &

# Kill the most recent task placed in the background
kill $!

# Kill the task be job number (in this case 1)
kill %1

# Output: I see sparks fly whenever you smile
node es5/main.js