# discfs
This is a content-addressable filesystem, whose storage backend is Discord.

# usage
You must first initialize an empty (or at least
one that has no pins) channel with `(init-channel <id>)`. This will
take something like 4 hours, because of Discord rate limiting.

Then, mount the channel: `(mount-channel <id>)`.

Then, it is possible to get a binary file already existing in the
filesystem with `(get <hash>)`, which returns an `'(unsigned-byte 8)`
stream. 

To delete a file, we do `(del <hash>)`. To put a file, we do `(put
stream)` where stream is some stream of bytes.

This is just for fun! Getting and putting files (especially large
ones) almost certaintly takes an inordinate amount of time, as Discord
has some pretty crazy rate limits.
