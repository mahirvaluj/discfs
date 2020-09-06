# discfs
This is a content-addressable filesystem, whose storage backend is Discord.

# usage
You must first initialize an empty (or at least
one that has no pins) channel with `(init-channel <id>)`. This will
take something like 4 hours, because of Discord rate limiting.

Then, mount the channel: `(mount-channel <id>)`.

Then, it is possible to get a binary file already existing in the
filesystem with `(get <hash-string>)`, which returns an `'(unsigned-byte 8)`
array. (Or, NIL on hash collision or other failure)

To delete a file, we do `(del <hash-string>)`. To put a file, we do
`(put stream)` or `(put array)` or `(put #p"/path/to/file")` passing either some stream of `(unsigned-byte 8)`, or an
array of `(unsigned-byte 8)`, or a pathname.

This is just for fun! Getting and putting files (especially large
ones) almost certaintly takes an inordinate amount of time, as Discord
has some pretty crazy rate limits.
