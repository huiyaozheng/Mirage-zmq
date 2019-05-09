import zmq
context = zmq.Context()
socket = context.socket(zmq.ROUTER)
socket.bind("tcp://127.0.0.1:5556")
for i in range(10):
    address, empty, ready = socket.recv_multipart()
    print(address)
    print(ready)
    socket.send_multipart([address, b'', b'workload'])
    