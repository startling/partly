## Partly

This is a haskell library and command-line tool for inspecting,
creating, and manipulating master boot records from disk images and (at
your own risk!) block devices.

`fdisk` has a few limitations, in my humble opinion -- it's written
to be used interactively, doesn't display things in machine-readable
format, and seems to be mostly centered around block devices.

`partly`, on the other hand, explicitly favors disk images and never
alters the file whose MBR you want to read.

## Usage

`partly view` is for inspecting and dumping the contents of an MBR.

* `partly json` gives you a number of ways to dump the boot record as JSON.
* `partly signature` simply prints the 16-bit boot signature as four
  hexadecimal digits.
* `partly bootloader` dumps the bootloader section (the first 446 bytes
  of the MBR) to a file or to stdout.
* `partly view partitions` pretty-prints the partition table, à la
  `fdisk`.
* `partly view timestamp` views the obsolete DOS-style
  [disk timestamp][]. N.B. that there may be false positives here.
* `partly view disk-signature` prints the optional [disk signature][] used by 
  Windows NT/2000/Vista/7 and some other things (?). N.B. that there
  may be false positives here, too, especially when those bytes are
  zeroed.
  
`partly make` is for creating MBRs, based on existing ones or not. You
can see the specific flags and arguments with `partly make --help`.

[disk timestamp]: http://thestarman.pcministry.com/asm/mbr/mystery.htm
[disk signature]: http://diddy.boot-land.net/firadisk/files/signature.htm

### But I really want to change the MBR of an image!

Godspeed and good luck.

````sh
partly make --from disk.img --some-options -o mbr.bin
dd if=mbr.bin of=disk.img bs=512 count=1 conv=notrunc
````

## Installation

You should probably have [GHC][] and [cabal-install][] installed,
firstly; your distribution (or homebrew, on OS X) probably provides
these. Then `cabal install partly` will install the latest release
from Hackage.

If you want to build from source, `cabal configure` and then `cabal
build` will build it to `dist/` and register it in place.

To run the tests, re-configure with `cabal configure --enable-tests`,
`cabal build`, and `cabal test`.

[GHC]: http://www.haskell.org/ghc/
[cabal-install]: http://hackage.haskell.org/trac/hackage/wiki/CabalInstall

## Donations

If you found this useful, I'd love if you [left me a tip][]. If not,
feel free to leave a [bug report][].

[left me a tip]: https://www.gittip.com/startling/
[bug report]: https://github.com/startling/partly/issues

## Todo

* map partition type codes to readable strings -- this is an expansive
  and inexact art. See [this page][partition-types].
* `partly view partition 1`, that dumps the contents of
  that partition from a disk image.
* `partly make --first-partition fs.img` to automagically tailor an
  MBR for a filesystem image.
* Support for GUID partition tables.

[partition-types]: http://www.win.tue.nl/~aeb/partitions/partition_types-1.html
