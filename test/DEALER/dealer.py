import zmq
context = zmq.Context()
socket = context.socket(zmq.DEALER)
socket.connect("tcp://127.0.0.1:5556")
for i in range(10):
    print("Sending request " + str(i))
    socket.send(b"", flags=zmq.SNDMORE)
    socket.send_string("Hello")
for i in range(10):
    message = socket.recv()
    print("Received reply: %s" % message)
