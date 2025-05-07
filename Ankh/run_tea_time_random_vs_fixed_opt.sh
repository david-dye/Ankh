#!/bin/bash
NUM_ITER=1000

echo "" > opt_fixed_output.csv
echo "" > opt_random_output.csv

# Run programs with randomized order to control for order effects
for i in $(seq 1 $NUM_ITER)
do
    # Randomly determine which program to run first (0 or 1)
    if [ $((RANDOM % 2)) -eq 0 ]; then
        # Run fixed version first
        ./tea_time_opt >> "opt_fixed_output.csv"
        ./tea_time_opt_random >> "opt_random_output.csv"
    else
        # Run random version first
        ./tea_time_opt_random >> "opt_random_output.csv"
        ./tea_time_opt >> "opt_fixed_output.csv"
    fi
done

# fixed 
# Average: 2.917870e-06
# Standard Deviation: 1.866930e-06
# Average: 3.396725e-06
# Standard Deviation: 3.854975e-06

# random 
# Average: 2.910466e-06
# Standard Deviation: 1.691150e-06
# Average: 3.359819e-06
# Standard Deviation: 4.196277e-06


# Control: we randomize execution order to minimize order effects 