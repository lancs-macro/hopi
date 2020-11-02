# This script clears the terminal, displays a greeting
# cd /drives/f/Themis/Repeated_Sales/
# clear terminal window

#echo "Removing commas, the double quotes."

awk -F'"' -v OFS='' '{ for (i=2; i<=NF; i+=2) gsub(",", "", $i) } 1' temp/raw.csv > temp/main.csv

#echo "Remove Hour"

sed -i 's/ 00:00//g' temp/main.csv

