echo "Building DEALER unikernel 1"
cd DEALER/unikernel
./build.sh
echo "Building DEALER unikernel 2"
cd DEALER/unikernel_bind
./build.sh
echo "Building unikernel for IDENTITY test"
cd ../../IDENTITY/unikernel
./build.sh
echo "Building PAIR unikernel 1"
cd ../../PAIR/unikernel1
./build.sh
echo "Building PAIR unikernel 2"
cd ../../PAIR/unikernel2
./build.sh
echo "Building client unikernel for PLAIN test"
cd ../../PLAIN/client_unikernel
./build.sh
echo "Building server unikernel for PLAIN test"
cd ../../PLAIN/server_unikernel
./build.sh
echo "Building PUB unikernel"
cd ../../PUB/unikernel
./build.sh
echo "Building PULL unikernel"
cd ../../PULL/unikernel
./build.sh
echo "Building PUSH unikernel"
cd ../../PUSH/unikernel
./build.sh
echo "Building PUSH unikernel for queue size test"
cd ../../Queue_size/PUSH_send_limited
./build.sh
echo "Building SUB unikernel for queue size test"
cd ../../Queue_size/SUB_receive_limited
./build.sh
echo "Building REP unikernel"
cd ../../REP/unikernel
./build.sh
echo "Building REQ unikernel"
cd ../../REQ/unikernel
./build.sh
echo "Building ROUTER unikernel 1"
cd ../../ROUTER/unikernel
./build.sh
echo "Building ROUTER unikernel 2"
cd ../../ROUTER/unikernel_connect
./build.sh
echo "Building SUB unikernel"
cd ../../SUB/unikernel
./build.sh
echo "Building XPUB unikernel"
cd ../../XPUB/unikernel
./build.sh
echo "Building XSUB unikernel"
cd ../../XSUB/unikernel
./build.sh