import zmq
import time
context = zmq.Context()
socket = context.socket(zmq.ROUTER)
socket.connect("tcp://127.0.0.1:5556")
time.sleep(1)
socket.send_multipart([b'router',b'Hello'])
address, msg = socket.recv_multipart()
print(msg)
    