# Discovery

Erlang dynamic node discovery based on network broadcasting.
Discovery is inspired from [nodefinder](https://github.com/bsmr-erlang/nodefinder.git).

## Internals:

After discovery app starts;

* Waits for interval time,
* Reads broadcast addres of given interface as env variable,
* Sends broadcast udp packet (that contains header, time, cookie, node info, ...)
* Receives udp response packets and process them.

...