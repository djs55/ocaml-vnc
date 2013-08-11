I need to figure out how much of the protocol can be encapsulated
in the library (for convenience) versus how much should be exposed
to allow interesting implementations.

The keyboard input events are actually X11 keysyms so we need a
wrapper around X11/keysymdef.h

A simple datastructure to remember dirtied rectangles would be
convenient. So would a means to coalesce adjacent or overlapping
small rectangles.

What should a server do if a client requests incremental updates for
a proper subrectangle of the buffer and then requests an incremental
update for the whole buffer?

