#!/bin/sh
~/faosync/pocketbooks/pocketbook_database/code/impute_syb/impute_syb1.R --args1 &
~/faosync/pocketbooks/pocketbook_database/code/impute_syb/impute_syb2.R --args2 &
~/faosync/pocketbooks/pocketbook_database/code/impute_syb/impute_syb3.R --args3 &
~/faosync/pocketbooks/pocketbook_database/code/impute_syb/impute_syb4.R --args4 &

wait
echo all processes complete