import zmq

context = zmq.Context()

#  Socket to talk to server
print("Starting server…")
socket = context.socket(zmq.ROUTER)
socket.connect("tcp://127.0.0.1:5556")
for i in range(10):
    address, empty, ready = socket.recv_multipart()
    print(ready)
    socket.send_multipart([address, b'', b'workload'])
    