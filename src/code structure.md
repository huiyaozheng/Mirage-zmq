# Implementation Code Struture

## Reference RFC

RFC 23/ZMTP defines the ZMTP protocol as follows

```
;   The protocol consists of zero or more connections
zmtp = *connection

;   A connection is a greeting, a handshake, and traffic
connection = greeting handshake traffic

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

;   The handshake consists of at least one command
;   The actual grammar depends on the security mechanism
handshake = 1*command

;   Traffic consists of commands and messages intermixed
traffic = *(command | message)

;   A command is a single long or short frame
command = command-size command-body
command-size = %x04 short-size | %x06 long-size
short-size = OCTET          ; Body is 0 to 255 octets
long-size = 8OCTET          ; Body is 0 to 2^63-1 octets
command-body = command-name command-data
command-name = OCTET 1*255command-name-char
command-name-char = ALPHA
command-data = *OCTET

;   A message is one or more frames
message = *message-more message-last
message-more = ( %x01 short-size | %x03 long-size ) message-body
message-last = ( %x00 short-size | %x02 long-size ) message-body
message-body = *OCTET
```

## Overarching structure

### `context.ml`

A `Context` contains a number of `Socket`s.

### `socket.ml`

`Socket` is the base module for other kinds of sockets. Each socket contains connection(s) and other supporting structures.

### `connection.ml`

`Connection` is the module signature of an endpoint-to-endpoint connection. Each connection has 3 stages: greeting, security mechanism handshake and message/command traffic. It feeds bytes from lower level to the state machines of each stage. 

### `connection_tcp.ml`

A `Connection_tcp` manages the lower level details of a port-to-port connection.

### `greeting.ml`

This FSM handles the 64-byte greeting. It validates the greeting (if version and mechanism agreed) or produces `Error` that will close the connection.

### `security_mechanism.ml`

This base module defines the signature of the FSM of a handshake stage.

### `traffic.ml`

This FSM handles the semantics of messages and commands.

## Concrete Implementations

### Security Mechanisms

#### `null_security_mechanism.ml`

This FSM handles the NULL security mechanism in 23/ZMTP.

#### `plain_security_mechanism.ml`

This FSM handles the security mechanism in 24/ZMTP-PLAIN.

### Sockets 

#### `req.ml`

This FSM implements the REQ socket defined in 28/REQREP.

#### `rep.ml`

This FSM implements the REP socket defined in 28/REQREP.

#### `dealer.ml`

This FSM implements the DEALER socket defined in 28/REQREP.

#### `router.ml`

This FSM implements the ROUTER socket defined in 28/REQREP.