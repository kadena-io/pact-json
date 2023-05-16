# Pact JSON Encoding

This library exposes the same API as
[`aeson`](https://hackage.haskell.org/package/aeson), and is intended for use
in serializing Pact values, which happens when dealing with API traffic and when
persisting data to the pact database.

It differs from `aeson` in the details of the encoding, using an encoding that
is compatible with data currently on the Kadena blockchain:

 - Objects are encoded using `aeson`'s legacy field ordering (which is different
   from contemporary `aeson` field ordering).
 - Infinite numbers are encoded as JSON strings "-inf" and "inf".
