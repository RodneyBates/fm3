#/usr/bin/bash

cat etestnames | xargs -n 1 -I % -- /usr/bin/bash ../onetest.sh $1 % 

