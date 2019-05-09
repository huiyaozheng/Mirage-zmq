cd PLAIN/client_unikernel
./req_unikernel 2> ../../output1.tmp & p1=$! 
cd ../server_unikernel
./rep_unikernel 2> ../../output2.tmp & p2=$! 
cd ../..
wait "$p1"
disown "$p2"
kill "$p2"
FD1=7
FD2=8
file="output1.tmp"
file_regex="PLAIN/client_unikernel/expected_output.txt"
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
file_regex="PLAIN/server_unikernel/expected_output.txt"
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
    echo "PLAIN passed"
else 
    echo "PLAIN failed"
fi
