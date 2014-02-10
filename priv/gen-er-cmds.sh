#!/bin/sh

here=`dirname $0`

grep "^(" $here/../include/redis-cmds.lfe |sed 's/)//g' | awk -F" " '{print $2}' | sort -n
