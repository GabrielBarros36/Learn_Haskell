#!/bin/bash

# Loop through all files in current directory
for file in *; do
    # Check if it's a regular file (not a directory) and doesn't start with a dot
    if [[ -f "$file" && ! "$file" =~ ^\. ]]; then
        # Check if it doesn't end with .hs or .md
        if [[ ! "$file" =~ \.hs$ && ! "$file" =~ \.md$ && ! "$file" =~ \.sh$ ]]; then
            # Delete the file
            rm "$file"
            echo "Deleted: $file"
        fi
    fi
done
