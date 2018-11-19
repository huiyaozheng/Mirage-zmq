# Implementation Code Struture

## Overarching structure

```
;   The protocol consists of zero or more connections
zmtp = *connection
```

### `context.ml`

A `Context` contains a number of `Socket`s. (The RFC does not mention 'context' but the 0MQ guide does.)

### `socket.ml`

`Socket` is the base module for other kinds of sockets. Each socket contains connection(s) and other supporting structures.

### `connection.ml`

```
;   A connection is a greeting, a handshake, and traffic
connection = greeting handshake traffic
```

`Connection` is the module signature of an endpoint-to-endpoint connection. Each connection has 3 stages: greeting, security mechanism handshake and message/command traffic. It feeds bytes from lower level to the state machines of each stage. 

### `connection_tcp.ml`

A `Connection_tcp` manages the lower level details of a port-to-port connection.

### `greeting.ml`

```
;   The greeting announces the protocol details
greeting = signature version mechanism as-server filler

signature = %xFF padding %x7F
padding = 8OCTET        ; Not significant

version = version-major version-minor
version-major = %x03
version-minor = %x00

;   The mechanism is a null padded string
mechanism = 20mechanism-char
mechanism-char = "A"-"Z" | DIGIT
    | "-" | "_" | "." | "+" | %x0

;   Is the peer acting as server?
as-server = %x00 | %x01

;   The filler extends the greeting to 64 octets
filler = 31%x00             ; 31 zero octets
```

This FSM handles the 64-byte greeting. It validates the greeting (if version and mechanism agreed) or produces `Error` that will close the connection.

### `security_mechanism.ml`

```
;   The handshake consists of at least one command
;   The actual grammar depends on the security mechanism
handshake = 1*command
```

This base module defines the signature of the FSM of a handshake stage.

### `traffic.ml`

```
;   Traffic consists of commands and messages intermixed
traffic = *(command | message)
```
This FSM handles the I/O message and command frames.

### `frame.ml`

```
;   A command is a single long or short frame
command = command-size command-body
command-size = %x04 short-size | %x06 long-size
short-size = OCTET          ; Body is 0 to 255 octets
long-size = 8OCTET          ; Body is 0 to 2^63-1 octets
command-body = command-name command-data
command-name = OCTET 1*255command-name-char
command-name-char = ALPHA
command-data = *OCTET
```

```
;   A message is one or more frames
message = *message-more message-last
message-more = ( %x01 short-size | %x03 long-size ) message-body
message-last = ( %x00 short-size | %x02 long-size ) message-body
message-body = *OCTET
```

`Frame` generates a complete frame from the given options and data.

## Concrete Implementations

### Security Mechanisms

#### `null_security_mechanism.ml`

```
null = ready *message | error
ready = command-size %d5 "READY" metadata
metadata = *property
property = name value
name = OCTET 1*255name-char
name-char = ALPHA | DIGIT | "-" | "_" | "." | "+"
value = 4OCTET *OCTET       ; Size in network byte order
error = command-size %d5 "ERROR" error-reason
error-reason = OCTET 0*255VCHAR
```

This FSM handles the NULL security mechanism in 23/ZMTP.

#### `plain_security_mechanism.ml`

```
plain = C:hello ( S:welcome | S:error )
        C:initiate ( S:ready | S:error )
     *( C:message | S:message )

hello = command-size %d5 "HELLO" username password
username = OCTET *VCHAR
password = OCTET *VCHAR

welcome = command-size %d7 "WELCOME"

initiate = command-size %d8 "INITIATE" metadata

ready = command-size %d5 "READY" metadata
```

This FSM handles the security mechanism in 24/ZMTP-PLAIN.

### Sockets 

#### `req.ml`

```
General behavior:

MAY be connected to any number of REP or ROUTER peers.
SHALL send and then receive exactly one message at a time.
The request and reply messages SHALL have this format on the wire:

A delimiter, consisting of an empty frame, added by the REQ socket.
One or more data frames, comprising the message visible to the application.
For processing outgoing messages:

SHALL prefix the outgoing message with an empty delimiter frame.
SHALL route outgoing messages to connected peers using a round-robin strategy.
SHALL block on sending, or return a suitable error, when it has no connected peers.
SHALL NOT discard messages that it cannot send to a connected peer.
For processing incoming messages:

SHALL accept an incoming message only from the last peer that it sent a request to.
SHALL discard silently any messages received from other peers.
```

This FSM implements the REQ socket defined in 28/REQREP.

#### `rep.ml`

```
General behavior:

MAY be connected to any number of REQ or DEALER peers.
SHALL not filter or modify outgoing or incoming messages in any way.
SHALL receive and then send exactly one message at a time.
The request and reply messages SHALL have this format on the wire:

An address envelope consisting of zero or more frames, each containing one identity.
A delimiter, consisting of an empty frame.
One or more data frames, comprising the message visible to the application.
For processing incoming messages:

SHALL receive incoming messages from its peers using a fair-queuing strategy.
SHALL remove and store the address envelope, including the delimiter.
SHALL pass the remaining data frames to its calling application.
For processing outgoing messages:

SHALL wait for a single reply message from its calling application.
SHALL prepend the address envelope and delimiter.
SHALL deliver this message back to the originating peer.
SHALL silently discard the reply, or return an error, if the originating peer is no longer connected.
SHALL not block on sending.
```

This FSM implements the REP socket defined in 28/REQREP.

#### `dealer.ml`

```
General behavior:

MAY be connected to any number of REP or ROUTER peers, and MAY both send and receive messages.
SHALL not filter or modify outgoing or incoming messages in any way.
SHALL maintain a double queue for each connected peer, allowing outgoing and incoming messages to be queued independently.
SHALL create a double queue when initiating an outgoing connection to a peer, and SHALL maintain the double queue whether or not the connection is established.
SHALL create a double queue when a peer connects to it. If this peer disconnects, the DEALER socket SHALL destroy its double queue and SHALL discard any messages it contains.
SHOULD constrain incoming and outgoing queue sizes to a runtime-configurable limit.
For processing outgoing messages:

SHALL consider a peer as available only when it has a outgoing queue that is not full.
SHALL route outgoing messages to available peers using a round-robin strategy.
SHALL block on sending, or return a suitable error, when it has no available peers.
SHALL not accept further messages when it has no available peers.
SHALL NOT discard messages that it cannot queue.
For processing incoming messages:

SHALL receive incoming messages from its peers using a fair-queuing strategy.
SHALL deliver these to its calling application.
```

This FSM implements the DEALER socket defined in 28/REQREP.

#### `router.ml`

```
General behavior:

MAY be connected to any number of REQ, DEALER, or ROUTER peers, and MAY both send and receive messages.
SHALL maintain a double queue for each connected peer, allowing outgoing and incoming messages to be queued independently.
SHALL create a double queue when initiating an outgoing connection to a peer, and SHALL maintain the double queue whether or not the connection is established.
SHALL create a double queue when a peer connects to it. If this peer disconnects, the ROUTER socket SHALL destroy its double queue and SHALL discard any messages it contains.
SHALL identify each double queue using a unique "identity" binary string.
SHOULD allow the peer to specify its identity explicitly through the Identity metadata property.
SHOULD constrain incoming and outgoing queue sizes to a runtime-configurable limit.
For processing incoming messages:

SHALL receive incoming messages from its peers using a fair-queuing strategy.
SHALL prefix each incoming message with a frame containing the identity of the originating double queue.
SHALL deliver the resulting messages to its calling application.
For processing outgoing messages:

SHALL remove the first frame from each outgoing message and use this as the identity of a double queue.
SHALL route the message to the outgoing queue if that queue exists, and has space.
SHALL either silently drop the message, or return an error, depending on configuration, if the queue does not exist, or is full.
SHALL NOT block on sending.
```

This FSM implements the ROUTER socket defined in 28/REQREP.