#!/bin/sh

# Start the first "count" processes and measure their execution time
c=0
for ((i=0; i<$1; i++)); do
    for ((j=0; j<10; j++)); do
        runhaskell -i"session4" "session4/test.hs" > /dev/null 2>&1 &
        c=$((c+1))
    done

    wait
done

echo ''$c' processes executed in '$SECONDS' seconds.'
# wait
