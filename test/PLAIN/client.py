import zmq
USER = b'admin'
PASS = b'password'
context = zmq.Context()
client = context.socket(zmq.REQ)
client.plain_username = USER
client.plain_password = PASS
client.connect('tcp://127.0.0.1:5556')
client.send(b'Hello')
msg = client.recv_string()
print(msg)