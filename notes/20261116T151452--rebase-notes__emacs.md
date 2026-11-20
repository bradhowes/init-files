---
title:      Rebase Notes
date:       2026-11-16T15:14:52+01:00
tags:       emacs
identifier: 
signature:  
---

# Goal

Add BlendedCriticalPaths stress test from commit d7d51080 to master.

# Branches

- master -- the one we are wishing to merge into with a PR
- xtc-run-d8d51080-exc-limit -- what has been used for testing
- backup/xtc-run-d8d51080-orig -- reconstitution of d8d51080 from delete branch
- backup/xtc-run-d8d51080 -- fixes to eliminate test failures

# Commits in backup/xtc-run-d8d51080

Starts with backup/xtc-run-d8d51080-orig

- e778b63a Fix failure due to unknown state at test start.
- 2deab081 Fix test failure. (move back to LSX for exchange instead of TIB used during IPO)
- 13fe1640 Markdown file documenting the code changes for the rebase with master
- 507b50d4 Report timeouts
- 24fd1115 Buy: derive trades per session from AVG_TRADES_PER_SESSION (needs work)
- e200695c BlendedCriticalPaths: update stale per-session constants in comments
- ced229a3 Browse: update stress multiplier comment to match 4800 rps
- 84126b2d Remove unused duplicated user-account CSVs (2)
- b6fe3e2f xtcRequest: throw RESTTimeoutException instead of JUnit fail()
- c7313c7f Use JVM 21 for Kotlin.

Master now has:

- efe961f7 Use HTTP v1.1 to overcome HTTP/2 client limit.
- 90bf52a1 Avoid exploding the Errors tab.


12099bc3 origin/xtc-run-d8d51080 Revert to master value.
330fdf0e origin/xtc-run-d7d51080-exc-limit Move CashBalance baseline rate into BlendedCriticalPaths
be307f58 Drop usersPerRps and maxUsersCeiling from Profile
efe961f7 Use HTTP v1.1 to overcome HTTP/2 client limit.
19c7e6ba adjust users per login


Rebase this branch on master

27cc2a1c backup/xtc-run-d8d51080-pre-rebase d8d51080-with-104912 Make blended-suite load parameters configurable per profile

This should now include:

90bf52a1 Avoid exploding the Errors tab.
efe961f7 Use HTTP v1.1 to overcome HTTP/2 client limit.

After, add:

be307f58 Drop usersPerRps and maxUsersCeiling from Profile
330fdf0e origin/xtc-run-d7d51080-exc-limit Move CashBalance baseline rate into BlendedCriticalPaths
12099bc3 origin/xtc-run-d8d51080 Revert to master value.

# Rebase Notes

- Commit c1d05126 more iterations per login:
  - CashBalance.kt
    - RUNS_PER_LOGIN changes from 10 to 40 in blendedTests
    - Added PRODUCTION_BASELINE_RATE_PER_SECOND = 200.0 but not used
  - Buy.kt
    - AVG_TRADES_PER_SESSION from 10 to 20
  - InstrumentChart.kt
    - RUNS_PER_LOGIN from 20 to 100
  - Search.kt
    - RUNS_PER_LOGIN from 5 to 50


- Commit 4a4ce4c6 separate user pools -- drop completely

# Log for xtc-run-d7d51080-exc-limit

94dcb49f *   xtc-run-d7d51080-exc-limit Merge remote-tracking branch 'origin/master' into xtc-run-d7d51080-exc-limit
         |\  
c3392d4d | * 4.9.117 master PTECH-106476: Avoid exploding the Errors tab. (#1485)
1efaded1 | * 4.9.116 PTECH-99405: Enable assert calls on XTC. (#1489)
98ec415f | * 4.9.115 PTECH-107150: Stress search at 2x production peak (#1488)
cf4fe9de | * 4.9.114 PTECH-104912: Add portfolio-chart write load suite (#1482)
269acaab * | backup/xtc-run-d7d51080-exc-limit-pre-rebase Revert to master value.
fbd2b41b * | Move CashBalance baseline rate into BlendedCriticalPaths
2cb12725 * | Drop usersPerRps and maxUsersCeiling from Profile
efe961f7 * | origin/xtc-run-d7d51080-exc-limit Use HTTP v1.1 to overcome HTTP/2 client limit.
19c7e6ba * | adjust users per login
90bf52a1 * | Avoid exploding the Errors tab.
27cc2a1c * | backup/xtc-run-d8d51080-pre-rebase d8d51080-with-104912 Make blended-suite load parameters configurable per profile
cf7ca11b * | Updated comments with links to code
d700068a * | Remove redundant duplicated user-account CSVs
2a8b2706 * | Remove stale branch-vs-master review doc
aac446f3 * |   Merge remote-tracking branch 'origin/xtc-run-d8d51080-orig' into xtc-run-d8d51080
         |\ \  
7f9c9e86 | * | xtc-run-d8d51080-orig origin/xtc-run-d8d51080-orig Merge origin/master into xtc-run-d8d51080-orig
         | |\| 
e86c13f3 | | * 4.9.113 PTECH-104820: Route cost-information and price-alarms alerts to equities channel (#1480)
cc4d908a | | * 4.9.112 PTECH-92682: raise order fees load to 800 req/s (#1479)
e95294ae | | * 4.9.111 PTECH-92682: order fees stress test (#1478)
4a6b1be4 | | * PTECH-102294: Make weekly scaleups reliable. (#1477)
fe25f6a9 | | * PTECH-86714: Replace GH_READ_REPOSITORIES_TOKEN with GitHub App token (#1476)
fac6c4c6 | | * 4.9.110 PTECH-93941: add quote burst load test for derivatives-key-figures (#1459)
8406574c | | * 4.9.109 PTECH-96038: Keep milliseconds in LTP CreDtTm (#1475)
322e5fde | | * 4.9.108 PTECH-99555: Size SEPA stress pools for surge runs (#1471)
98f68fbe | | * 4.9.107 PTECH-93941: Publish several Kafka messages in one Bento request (#1474)
679b520d | | * 4.9.106 PTECH-99557: Add incoming SCT E2E test (#1472)
6ff6b2d4 | | * 4.9.105 PTECH-92682: Add CIS order-fees load test (#1468)
bd8c67bf | | * 4.9.104 PTECH-99405: No longer make Booking test so slow. (#1470)

# Log for backup/blended-rebase

6c3767a7 * backup/blended-rebase Stop using duplicated accounts.
932d4b33 * Fix failure due to unknown state at test start.
57dc5a5b * Update stress profile per second value
7f5046f1 * Browse: move to /api/v2/browse and add serial IPO hub call
518ef5fb * include ipo access topic in blended test
5ac3a96e * restore instrument chart to 5x typical
da8f2afd * restore special user pool for cash balance
469744fc * no duplicate user accounts for buy
6b9ac48d * timeout on brwose details
d8e921db * separate user pools
6e9b0cc6 * duplicate users for heavily concurrent tests
a63b26c0 * no IPO load
f6370f3e * more iterations per login
147b8924 * more global vus
24dbc4ce * more vus
a62e7de6 * tickers from LSX
04ae118f * vu pool increase
777aab66 * blended fixes
f5786a33 * blend with ipo
bb3f4179 * Add blended critical-paths concurrent load-test suite
ec3fc5f0 * TIB
c3392d4d * 4.9.117 master PTECH-106476: Avoid exploding the Errors tab. (#1485)
1efaded1 * 4.9.116 PTECH-99405: Enable assert calls on XTC. (#1489)
98ec415f * 4.9.115 PTECH-107150: Stress search at 2x production peak (#1488)
cf4fe9de * 4.9.114 PTECH-104912: Add portfolio-chart write load suite (#1482)
e86c13f3 * 4.9.113 PTECH-104820: Route cost-information and price-alarms alerts to equities channel (#1480)
cc4d908a * 4.9.112 PTECH-92682: raise order fees load to 800 req/s (#1479)
e95294ae * 4.9.111 PTECH-92682: order fees stress test (#1478)
4a6b1be4 * PTECH-102294: Make weekly scaleups reliable. (#1477)
fe25f6a9 * PTECH-86714: Replace GH_READ_REPOSITORIES_TOKEN with GitHub App token (#1476)
fac6c4c6 * 4.9.110 PTECH-93941: add quote burst load test for derivatives-key-figures (#1459)
8406574c * 4.9.109 PTECH-96038: Keep milliseconds in LTP CreDtTm (#1475)
322e5fde * 4.9.108 PTECH-99555: Size SEPA stress pools for surge runs (#1471)
98f68fbe * 4.9.107 PTECH-93941: Publish several Kafka messages in one Bento request (#1474)
679b520d * 4.9.106 PTECH-99557: Add incoming SCT E2E test (#1472)
6ff6b2d4 * 4.9.105 PTECH-92682: Add CIS order-fees load test (#1468)

# Parameters

The `generated/other:blendedCriticalPaths-stress.properties` contains the default values that will be used when running
the `other:blendedCriticalPaths` test suite.

Examples:

```
com.xceptance.xlt.loadtests.default.measurementPeriod = 1h
com.xceptance.xlt.loadtests.Search.users = 2000

com.xceptance.xlt.loadtests.Search.users = 12000
com.xceptance.xlt.loadtests.IpoAccessBrowse.users = 5000
com.xceptance.xlt.loadtests.IpoSubscription.users = 5000

com.xceptance.xlt.loadtests.Buy.arrivalRate = 7200
com.xceptance.xlt.loadtests.CryptoBuy.arrivalRate = 7200
```

