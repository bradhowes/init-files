---
title:      Rebase Notes
date:       2026-11-16T15:14:52+01:00
tags:       emacs
identifier: 
signature:  
---

12099bc3 origin/xtc-run-d8d51080 Revert to master value.
330fdf0e origin/xtc-run-d7d51080-exc-limit Move CashBalance baseline rate into BlendedCriticalPaths
be307f58 Drop usersPerRps and maxUsersCeiling from Profile
efe961f7 Use HTTP v1.1 to overcome HTTP/2 client limit.
19c7e6ba adjust users per login
90bf52a1 Avoid exploding the Errors tab.


Rebase this branch on master

27cc2a1c backup/xtc-run-d8d51080-pre-rebase d8d51080-with-104912 Make blended-suite load parameters configurable per profile

This should now include:

90bf52a1 Avoid exploding the Errors tab.
efe961f7 Use HTTP v1.1 to overcome HTTP/2 client limit.

After, add:

be307f58 Drop usersPerRps and maxUsersCeiling from Profile
330fdf0e origin/xtc-run-d7d51080-exc-limit Move CashBalance baseline rate into BlendedCriticalPaths
12099bc3 origin/xtc-run-d8d51080 Revert to master value.
