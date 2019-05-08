import zmq
context = zmq.Context()
socket = context.socket(zmq.REQ)
socket.connect("tcp://localhost:5556")
for i in range(10):
    socket.send(b"Hello")
    message = socket.recv()
    print("Received reply: %s" % message)