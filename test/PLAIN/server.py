import zmq
import zmq.auth
from zmq.auth.thread import ThreadAuthenticator
context = zmq.Context()
server = context.socket(zmq.REP)

auth = ThreadAuthenticator(context)
auth.start()
auth.allow('127.0.0.1')
auth.configure_plain(domain='*', passwords={'admin': 'password'})
server.plain_server = True
server.setsockopt(zmq.PLAIN_SERVER, 1)
server.connect('tcp://127.0.0.1:5556')

msg = server.recv_string()
server.send(b'Authenticated')

auth.stop()

