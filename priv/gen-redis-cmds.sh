#!/bin/sh

REDIS_C=~/repos/redis/src/redis.c

grep "^    {" $REDIS_C |grep "}.\?$" | sed -e 's/    //g' -e 's/"//g' -e 's/{//g' -e 's/}//g' | awk -F, '{print $1}' | sort -n

# grep "^    {" $REDIS_C |grep "}.\?$" | sed -e 's/    //g' -e 's/"//g' -e 's/{//g' -e 's/}//g' | awk -F, '{ if($3>0) {print $1,$3-1} else {print $1,$3+1}}' | sort -n > CMDS_WITH_ARITY
#cmds= -- copy/pasted output from the grep above
#for c in $cmds; do echo -n "(newcmd "; grep "^$c " CMDS_WITH_ARITY | tr -d '\n'; echo ")"; don
