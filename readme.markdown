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
  
`partly make` is for creating MBRs, based on existing ones or not. You
can see the specific flags and arguments with `partly make --help`.

### But I really want to change the MBR of an image!

Godspeed and good luck.

````sh
partly make --from disk.img --some-options -o mbr.bin
dd if=mbr.bin of=disk.img bs=512 count=1 conv=notrunc
````
    
## Todo

* map partition type codes to readable strings -- this is an expansive
  and inexact art. See [this page][partition-types].
* `partly view partition 1`, that dumps the contents of
  that partition from a disk image.
* `partly make --first-partition fs.img` to automagically tailor an
  MBR for a filesystem image.
* Support for GUID partition tables.
* Add support for viewing the DOS-style timestamp and disk signatures
  in the command-line tool.

[partition-types]: http://www.win.tue.nl/~aeb/partitions/partition_types-1.html
