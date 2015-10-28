# holborn-api

This service deals with internal and external API requests. At the
time of writing the only API is the internal auth + dispatch API.

## Problem to be solved

OpenSSH is a dumb terminator and doesn't know where to send
traffic. All it does is ask for a public key, run the DH handshake,
and then exec a command.

holborn-api can send any command it wants depending on the information
sent by openssh (e.g. it can print a message like `"rate limit
exceeded"` and close the connection).

The most common command though is a bidirectional pipe to
holborn-repo's Raw API with `nc`. Each holborn-repo server is
responsible for many repositories so we need to tell the Raw API which
specific repository the ssh command asked for. This is done by
prefixing the bidirectional traffic with a small JSON dictionary
containing the required metadata.
