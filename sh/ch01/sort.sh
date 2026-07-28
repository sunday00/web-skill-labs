#sort ./f3.txt

#sort -n ./f3.txt

#sort -r ./f2.txt

#sort -u ./f2.txt # unique

#sort ./f4.txt
#echo '==============='
#sort -n ./f4.txt
#echo '==============='
#sort -h ./f4.txt # interpret kilo, million, billion text - like K, M, B...

#sort -t ',' -k 1 ./users.csv
#echo '==============='
#sort -t ',' -k 2 ./users.csv
#echo '==============='
#sort -t ',' -k 3 ./users.csv # sort with 3 and next all field. using 4~ field when 3rd field is same
#echo '==============='
#sort -t ',' -k 3,3 ./users.csv # sort with only 3 field
#echo '==============='
#sort -t ',' -k 2,3 ./users.csv # sort with 2-3 field
#echo '==============='
#sort -t ',' -k 1,1 -k 3,3 ./users.csv # sort with 1, 3 field

ls -al | sort -k5 -n # sort files with size