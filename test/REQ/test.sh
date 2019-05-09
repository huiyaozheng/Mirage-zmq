cd REQ/unikernel
./req_unikernel 2> ../../output1.tmp & p1=$! 
cd ../../REP
python3 rep.py > ../output2.tmp & p2=$! 
cd ..
wait "$p1"
disown "$p2"
kill "$p2"
FD1=7
FD2=8
file="output1.tmp"
file_regex="REQ/unikernel/expected_output.txt"
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
file_regex="REP/expected_output.txt"
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
    echo "REQ unikernel - REP Pyzmq passed"
else 
    echo "REQ unikernel - REP Pyzmq failed"
fi

cd REQ/unikernel
./req_unikernel 2> ../../output1.tmp & p1=$! 
cd ../../REP/unikernel
./rep_unikernel 2> ../../output2.tmp & p2=$! 
cd ../..
wait "$p1"
disown "$p2"
kill "$p2"
FD1=7
FD2=8
file="output1.tmp"
file_regex="REQ/unikernel/expected_output.txt"
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
file_regex="REP/unikernel/expected_output.txt"
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
    echo "REQ unikernel - REP unikernel passed"
else 
    echo "REQ unikernel - REP unikernel failed"
fi

cd REQ/unikernel
./req_unikernel 2> ../../output1.tmp & p1=$! 
cd ../../ROUTER
python3 router.py > ../output2.tmp & p2=$! 
cd ..
wait "$p1"
disown "$p2"
kill "$p2"
FD1=7
FD2=8
file="output1.tmp"
file_regex="REQ/unikernel/expected_output.txt"
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
file_regex="ROUTER/expected_output.txt"
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
    echo "REQ unikernel - ROUTER Pyzmq passed"
else 
    echo "REQ unikernel - ROUTER Pyzmq failed"
fi

cd REQ/unikernel
./req_unikernel 2> ../../output1.tmp & p1=$! 
cd ../../ROUTER/unikernel
./router_unikernel 2> ../../output2.tmp & p2=$! 
cd ../..
wait "$p1"
disown "$p2"
kill "$p2"
FD1=7
FD2=8
file="output1.tmp"
file_regex="REQ/unikernel/expected_output.txt"
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
rm output1.tmp
rm output2.tmp
if $pass 
then 
    echo "REQ unikernel - ROUTER unikernel passed"
else 
    echo "REQ unikernel - ROUTER unikernel failed"
fi