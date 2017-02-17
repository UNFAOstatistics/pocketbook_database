#!/bin/sh
~/faosync/pocketbooks/pocketbook_database/code/aggregate_functions/EconomicAggregates.R --args1 &
~/faosync/pocketbooks/pocketbook_database/code/aggregate_functions/FAOAggregates.R --args2 &
~/faosync/pocketbooks/pocketbook_database/code/aggregate_functions/M49aggregates.R --args3 &
~/faosync/pocketbooks/pocketbook_database/code/aggregate_functions/SofiAggregates.R --args4 &

wait
echo all processes complete