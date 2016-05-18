#!/bin/sh

logfile=$(date -u +%Y-%m-%d-%H-%M-%S)-$$

./testrng "$@" | tee logs/$logfile
sed -n -e /^$/d -e '/Summary results/,$p' logs/$logfile >tmp-$$
cat tmp-$$ >>log-summary
rm -f tmp-$$
