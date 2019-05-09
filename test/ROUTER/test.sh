cd ROUTER/unikernel
./router_unikernel 2> ../../output1.tmp & p1=$! 
cd ../../REQ
python3 req.py > ../output2.tmp & p2=$! 
cd ..
wait "$p2"
disown "$p1"
kill "$p1"
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
    echo "ROUTER unikernel - REQ Pyzmq passed"
else 
    echo "ROUTER unikernel - REQ Pyzmq failed"
fi

cd ROUTER/unikernel
./router_unikernel 2> ../../output1.tmp & p1=$! 
cd ../../REQ/unikernel
./req_unikernel 2> ../../output2.tmp & p2=$! 
cd ../..
wait "$p2"
disown "$p1"
kill "$p1"
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
rm output1.tmp
rm output2.tmp
if $pass 
then 
    echo "ROUTER unikernel - REQ unikernel passed"
else 
    echo "ROUTER unikernel - REQ unikernel failed"
fi

cd ROUTER/unikernel
./router_unikernel 2> ../../output1.tmp & p1=$! 
cd ../../DEALER
python3 dealer.py > ../output2.tmp & p2=$! 
cd ..
wait "$p2"
disown "$p1"
kill "$p1"
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
file_regex="DEALER/expected_output_against_router.txt"
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
    echo "ROUTER unikernel - DEALER Pyzmq passed"
else 
    echo "ROUTER unikernel - DEALER Pyzmq failed"
fi

cd ROUTER/unikernel
./router_unikernel 2> ../../output1.tmp & p1=$! 
cd ../../DEALER/unikernel
./dealer_unikernel 2> ../../output2.tmp & p2=$! 
cd ../..
wait "$p2"
disown "$p1"
kill "$p1"
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
file_regex="DEALER/unikernel/expected_output.txt"
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
    echo "ROUTER unikernel - DEALER unikernel passed"
else 
    echo "ROUTER unikernel - DEALER unikernel failed"
fi

cd ROUTER/unikernel
./router_unikernel 2> ../../output1.tmp & p1=$! 
cd ..
python3 router_connect.py > ../output2.tmp & p2=$! 
cd ..
wait "$p2"
disown "$p1"
kill "$p1"
FD1=7
FD2=8
file="output1.tmp"
file_regex="ROUTER/unikernel/expected_output_against_router.txt"
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
file_regex="ROUTER/expected_output_against_router.txt"
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
    echo "ROUTER unikernel - ROUTER Pyzmq passed"
else 
    echo "ROUTER unikernel - ROUTER Pyzmq failed"
fi

cd ROUTER/unikernel
./router_unikernel 2> ../../output1.tmp & p1=$! 
cd ../unikernel_connect
./router_unikernel 2> ../../output2.tmp & p2=$! 
cd ../..
wait "$p2"
disown "$p1"
kill "$p1"
FD1=7
FD2=8
file="output1.tmp"
file_regex="ROUTER/unikernel/expected_output_against_router.txt"
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
file_regex="ROUTER/unikernel_connect/expected_output.txt"
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
    echo "ROUTER unikernel - ROUTER unikernel passed"
else 
    echo "ROUTER unikernel - ROUTER unikernel failed"
fi