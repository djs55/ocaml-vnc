I need to figure out how much of the protocol can be encapsulated
in the library (for convenience) versus how much should be exposed
to allow interesting implementations.

I need to remove the custom marshalling functions in lib/rfb.ml
and replace with Cstruct/bigarray-style marshalling.
  -- should we pre-allocate buffers per connection and use these
     for all updates to prevent excessive memory recycling?

The library only supports 32-bit pixel formats but clients are
allowed to request something different.

The keyboard input events are actually X11 keysyms so we need a
wrapper around X11/keysymdef.h

A simple datastructure to remember dirtied rectangles would be
convenient. So would a means to coalesce adjacent or overlapping
small rectangles.

The hextile and RRE encodings are missing. These would probably
be useful for transmitting black rectangles in the framebuffer example.

What should a server do if a client requests incremental updates for
a proper subrectangle of the buffer and then requests an incremental
update for the whole buffer?

