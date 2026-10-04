---
title:      OpenVSIP Compilation
date:       2024-06-08 12:20:22
tags:       code
identifier: "20240608T122022"
---
# OpenVSIP Compilation

```
rm -r objdir
mkdir $_
cd $_
../configure --with-lapack=apple --with-fftw-prefix=/opt/homebrew CXXFLAGS=-std=c++17
```

# Using Armadillo

Site - https://gitlab.com/conradsnicta/armadillo-code

git clone https://gitlab.com/conradsnicta/armadillo-code.git

## Installation

Uses Cmake so do the usual:

```
% mkdir build
% cd $_
% cmake -DALLOW\_OPENBLAS\_MACOS=ON -DCMAKE\_INSTALL\_PREFIX:PATH=/opt/homebrew .
% make 
% make install
```

The above will use the Accelerate framework as long as OpenBLAS is not located use `-DALLOW\_OPENBLAS\_MACOS=OFF` to
disable that.

