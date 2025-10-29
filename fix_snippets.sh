#!/bin/bash
for f in cparser/examples/pascal_parser/pascal/*.pas; do
    if ! builddir/pascal_parser_cli "$f" > /dev/null 2>&1; then
        echo "Fixing $f"
        # Use a temporary file to avoid issues with in-place editing
        tmpfile=$(mktemp)
        # Check if the file already has a unit declaration
        if grep -qi "unit " "$f"; then
            # If it does, just add implementation and end.
            cat "$f" > "$tmpfile"
            echo "implementation" >> "$tmpfile"
            echo "end." >> "$tmpfile"
        else
            # If not, wrap the whole file
            echo "unit $(basename "$f" .pas);" > "$tmpfile"
            echo "interface" >> "$tmpfile"
            cat "$f" >> "$tmpfile"
            echo "implementation" >> "$tmpfile"
            echo "end." >> "$tmpfile"
        fi
        mv "$tmpfile" "$f"
    fi
done
