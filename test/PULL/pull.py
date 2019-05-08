import zmq
import time
context = zmq.Context()
socket = context.socket(zmq.PULL)
socket.connect("tcp://127.0.0.1:5556")
for i in range(0, 10):
    work_item = socket.recv_string()
    print(work_item)
    