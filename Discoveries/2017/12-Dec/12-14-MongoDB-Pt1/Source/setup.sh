sudo mongo

# Executed within the mongodb shell in bash
# Change the database - a namespace in mongodb
use xmas

# Executed in Bash - set the EDITOR variable to vim so we can edit variables/functions
# in the mongodb shell in vim
export EDITOR=vim

# Executed within the mongodb shell
# Edit a function in vim
edit createTree

# Save the changes in vim - wq means write-quit
:wq