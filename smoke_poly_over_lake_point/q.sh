#!/bin/bash

for year in $(seq 2005 2025); do
    echo -n "Processing year $year … "

    sed "s/{{YEAR}}/$year/g" q.sql | psql -d spatial -o "smokes_lakes_points_composite${year}.csv"

    echo "done."
done

tail -n +5 "smokes_lakes_points_composite2005.csv" > "smokes_lakes_points_composite.csv"

for year in $(seq 2006 2025); do
    tail -n +6 "smokes_lakes_points_composite${year}.csv" >> "smokes_lakes_points_composite.csv"
done