# This script clears the terminal, displays a greeting
# cd /drives/f/Themis/Repeated_Sales/
# clear terminal window
clear

echo "The script starts now."

# echo "Downloading the data."

curl http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-complete.csv

echo "Removing commas, the double quotes."

awk -F'"' -v OFS='' '{ for (i=2; i<=NF; i+=2) gsub(",", "", $i) } 1' data/raw.csv > data/main.csv

echo "Remove Hour"

sed -i 's/ 00:00//g' lrdata/main.csv

