cd IDENTITY/unikernel
./req_unikernel 2> ../../output1.tmp & p1=$! 
cd ..
python3 router.py > ../output2.tmp & p2=$! 
cd ..
wait "$p1"
disown "$p2"
kill "$p2"
FD1=7
FD2=8
file="output1.tmp"
file_regex="ROUTER/unikernel/expected_output.txt"
pass=true
exec 7<$file
exec 8<$file_regex
while read data1 <&$FD1; do
	read data2 <&$FD2;
    if grep -q "$data2" <<< "$data1"; 
    then 
        : 
    else 
        pass=false 
    fi
done
FD1=7
FD2=8
file="output2.tmp"
file_regex="REQ/expected_output.txt"
pass=true
exec 7<$file
exec 8<$file_regex
while read data1 <&$FD1; do
	read data2 <&$FD2;
    if grep -q "$data2" <<< "$data1"; 
    then 
        : 
    else 
        pass=false 
    fi
done
rm output1.tmp
rm output2.tmp
if $pass 
then 
    echo "IDENTITY passed"
else 
    echo "IDENTITY failed"
fi
