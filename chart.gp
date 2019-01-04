set terminal pngcairo size 640,1280
set output "output.png"
set datafile separator " "
stats 'data' using 0 nooutput
set key autotitle columnheader
set timefmt "%Y-%m-%d %H:%M:%S"
set ydata time
set yrange ["2019-01-04 01:03:00":"2019-01-04 02:50:30"]
set key off
plot for [i=0:(STATS_blocks - 1)] 'data' using 1:2 index i with lines