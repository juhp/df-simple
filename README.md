# df-combine

Simplifies:

```shellsession
$ df
Filesystem     1K-blocks      Used Available Use% Mounted on
overlay        498426880 197677204 298827180  40% /
devtmpfs            4096         0      4096   0% /dev
tmpfs           16283276        84  16283192   1% /dev/shm
tmpfs            3256652      3444   3253208   1% /etc/hostname
tmpfs            6513312     10680   6502632   1% /run/host/run
/dev/dm-0      498426880 197677204 298827180  40% /run/host
tmpfs           16283280         8  16283272   1% /tmp
/dev/dm-0      498426880 197677204 298827180  40% /run/host/var
/dev/dm-0      498426880 197677204 298827180  40% /run/host/var/home
tmpfs            6513312     10680   6502632   1% /run/media
tmpfs            3256652      3444   3253208   1% /run/user/1000
/dev/nvme0n1p2    996780    174444    753524  19% /run/host/boot
/dev/nvme0n1p1    613184     39956    573228   7% /run/host/boot/efi
/dev/sda1      479546772 420492992  34620696  93% /var/mnt/extreme
```

to:

```shellsession
$ df-combine
Filesystem     1K-blocks      Used Available Use% Mounted on
/dev/nvme0n1p1    613184     39956    573228   7% /run/host/boot/efi
/dev/nvme0n1p2    996780    174444    753524  19% /run/host/boot
overlay        498426880 197679900 298824548  40% /
/dev/dm-0      498426880 197679900 298824548  40% /run/host, /run/host/var, /run/host/var/home
/dev/sda1      479546772 420492992  34620696  93% /var/mnt/extreme
```

ie sorts by Used, subgrouping filesystems combining mountpoints,
and hiding tmpfs.
