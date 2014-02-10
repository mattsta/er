#!/bin/bash

here=`dirname $0`
redis=`$here/gen-redis-cmds.sh`
er=`$here/gen-er-cmds.sh`

# This isn't a text diff because `diff -u` had problems
# properly lining command differences.

# For each REDIS_CMD, see if there's a matching ER_CMD
# If so, do nothing.  If no match found, print the
# missing REDIS_CMD

function two_compare() {
    outer="$1"
    inner="$2"
    for outer_cmd in $outer; do
        for inner_cmd in $inner; do
            if [[ $outer_cmd == $inner_cmd ]]; then
                # break out of two levels to continue:
                continue 2
            fi
        done
        echo $outer_cmd
    done
}

echo "Redis commands not existing in ER:"
two_compare "$redis" "$er"
echo
echo "ER commands not existing in Redis:"
two_compare "$er" "$redis"
