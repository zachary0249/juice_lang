#!/bin/bash

# Check if the user provided a command-line argument for the parameter
if [ -z "$1" ]; then
  echo "Usage: $0 <command_parameter>"
  exit 1
fi

# The command-line argument will be the value for the '-c' parameter
command_param="$1"

# Loop over all files in the current test_input directory
for file in test_input/*; do
  # Check if it's a regular file (not a directory)
  if [ -f "$file" ]; then
      echo "$file"
    # Execute the dune command with the file and the given parameter
    dune exec juice -- -c "$command_param" -f "$file"
  fi
done
exit 0
