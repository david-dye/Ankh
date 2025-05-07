NUM_ITER=100
echo "" > opt_output.csv
echo "" > no_opt_output.csv

for i in $(seq 1 $NUM_ITER)
do
    ./tea_time_opt >> opt_output.csv
    ./tea_time_no_opt >> no_opt_output.csv
done