---
title:      Installing libdvdcss on macOS 15.0 Sequoia
date:       2024-09-29 21:41:04
tags:       dvd  handbrake
identifier: "20240929T214104"
---

```shell
% brew install autoconf
% brew install automake
% cd ~/src
% git clone https://code.videolan.org/videolan/libdvdcss.git
% cd libdvdcss
# From INSTALL instructions:
% autoreconf -i
% ./configure --prefix=/usr/local
% make
% sudo make install
```

Download handbrake if necessary.
It should now read directly from a DVD without any issue.
