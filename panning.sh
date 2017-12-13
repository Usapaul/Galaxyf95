#!/bin/bash

declare -a coords 
exec 10<onlyxy.dat
let numobj=0
while read LINE <&10; do
	coords[$numobj]=$LINE
	((numobj++))
done
exec 10>&-

echo `rm -f comments.dat`
echo `touch comments.dat`
echo "Make a comment for each selected object."
echo "The following comment will be a good mark for a galaxy: 'ok'"
echo "And this is a bad mark: 'no'"
echo "Other comments will be saved too,"
echo "but objects with 'no' will be removed"
echo " "
echo " "

echo "Press Enter when ds9 is opened (not earlier)"
read continuing

echo "Number of objects: ${numobj}"

for ((i=0;i<$numobj;i++)); do
	let jj=${i}+1
	echo `xpaset -p ds9 pan to ${coords[${i}]}; xpaset -p ds9 frame next; xpaset -p ds9 pan to ${coords[${i}]}`
	echo "Enter your comment for the object ${jj}/${numobj} (${coords[${i}]} )"
	read comment
	if [[ -z comment ]]; then
		comment="--"
	fi
	echo `echo $comment >> comments.dat`
done

