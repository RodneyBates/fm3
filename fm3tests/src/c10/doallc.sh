#/usr/bin/bash

cat ctestnames | xargs -n 1 -I % -- /usr/bin/bash ../onetest.sh $1 % 
