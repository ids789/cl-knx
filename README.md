# CL-KNX
A KNXD client for Common Lisp using CFFI and the KNXD C library

### Prerequisites
A [knxd](https://github.com/knxd/knxd) server with the knxd c library

Compile KNXD with shared libraries  `./configure --enable-shared` and ensure that libeibclient is available on under the `LD_LIBRARY_PATH` (found in the KNXD build directory under `knxd/src/client/c/.libs/libeibclient.so`)

##### knx-send
Sends a message on the KNX bus: `(knx-send url group message)`
* `url` is a url string to a knxd service e.g. `"ip:192.168.1.15:1234"`
* `group` is a list containing the destination group address e.g. `'(1 2 3)`
* `message` is a list generated by the `msg-encode` function

##### knx-read
Request a KNX group's value: `(knx-read url group [age])`
* `url` is a url string to a knxd service e.g. `"ip:192.168.1.15:1234"`
* `group` is a list containing the destination group address e.g. `'(1 2 3)`
* `age` is an optional paramter to set the maximum (seconds) a value can be cached before it needs to be re-read
* Returns a message list which can be decoded with the `msg-decode` function

##### knx-listen
Listen for any telegram from the KNX bus: `(knx-listen url)`
* `url` is a url string to a knxd service e.g. `"ip:192.168.1.15:1234"`
* Returns multiple values:
    1. The telegram payload which can be decoded with the `msg-decode` function
    2. The telegram type: `request`, `response` or `write`
    3. A list containing the destination group
    4. A list containing the address of the source device

##### msg-encode
Converts a value into a knx telegram payload: `(msg-encode value type)`
* `value` is the value to encode
* `type` is a symbol specifying the knx dpt type for the value
*  Returns a payload list which can be sent using `knx-send`

##### msg-decode
Convert received telegram payload into a lisp value: `(msg-decode message type)`
* `message` is a received payload list
* `type` is a symbol specifying the knx dpt type of the value
*  Returns a lisp value


##### Message Types
* `dpt-1.*` is for a boolean value, either `t` or `nil`
* `dpt-5.001` is for a percentage value between 0 and 100
* `dpt-17.001` is for a scene value between 1 and 50
* `dpt-9.*` is for a 16bit floating point number
* `dpt-14.*` is for a 32bit floating point number
* `dpt-13.*` is fora 32bit signed integer

### Notes
* Currently only supports using 3 level group addresses and 3 level device addresses
