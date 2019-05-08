import zmq
import time
context = zmq.Context()
socket = context.socket(zmq.PAIR)
socket.connect("tcp://127.0.0.1:5556")
work_item = socket.recv_string()
print(work_item)
socket.send_string("Hi there")
print("Greeting sent")