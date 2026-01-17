#!/bin/bash

for year in $(seq 2005 2025); do
    echo -n "Processing year $year … "

    sed "s/{{YEAR}}/$year/g" q.sql | psql -d spatial -o "wfrtl_composite${year}.csv"

    echo "done."
done