# df-simple

Simplifies:

```shellsession
$ df
Filesystem     1K-blocks      Used Available Use% Mounted on
overlay        498426880 215689652 281288780  44% /
devtmpfs            4096         0      4096   0% /dev
tmpfs           16283276        84  16283192   1% /dev/shm
tmpfs            3256652     13912   3242740   1% /etc/hostname
tmpfs            6513312     10692   6502620   1% /run/host/run
/dev/dm-0      498426880 215689652 281288780  44% /run/host
tmpfs           16283280         8  16283272   1% /tmp
/dev/dm-0      498426880 215689652 281288780  44% /run/host/var
/dev/dm-0      498426880 215689652 281288780  44% /run/host/var/home
tmpfs           16283276        84  16283192   1% /dev/shm
tmpfs            6513312     10692   6502620   1% /run/media
tmpfs            3256652     13912   3242740   1% /run/user/1000
/dev/nvme0n1p2    996780    260240    667728  29% /run/host/boot
/dev/nvme0n1p1    613184     39956    573228   7% /run/host/boot/efi
/dev/sda1      479546772 427327340  27786348  94% /var/mnt/extreme
```

to:

```shellsession
$ df-simple
Filesystem     Size Used Free  Use Mount
/dev/nvme0n1p1 613M  40M 573M   7% /run/host/boot/efi
/dev/nvme0n1p2 997M 260M 668M  29% /run/host/boot
/dev/dm-0      498G 216G 281G  44% overlay /; /run/host{,/var{,/home}}
/dev/sda1      480G 427G  28G  94% /var/mnt/extreme
```

ie sorts by Used, subgrouping filesystems combining mountpoints,
and hiding tmpfs.
