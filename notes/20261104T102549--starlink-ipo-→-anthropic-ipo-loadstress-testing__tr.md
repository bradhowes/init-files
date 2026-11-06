---
title:      Starlink IPO → Anthropic IPO Load/Stress Testing
date:       2026-11-04T10:25:49+01:00
tags:       tr
identifier: 
signature:  
---

# Starlink IPO → Anthropic IPO Load/Stress Testing

- [E2E Testing Intro][e2e]
- [XTC "blend" Past Runs][xtc]

# Recreating Repository

Chose to use the [latest "blended" report][run] in XTC. The report says that the git commit was 
`d8d510805e5a8bc1f3134b0ea2b9f8137eb43044`.

Claude recreated a branch locally with `d8d510805e5a8bc1f3134b0ea2b9f8137eb43044`. Initial attempt to build and run
tests was fairly successful, with 45 test failures.

Tests reported authentication issues. Needed to setup a `~/.gradle/gradle.properties`. There is a [template file][tf] to
use with instructions.

- secret.general.firewallHeader
- secret.general.instrumentUniverse.password
- secret.general.dalek.password
- secret.general.onboardingHelper.password
- secret.general.bento.basic.auth.password
- secret.general.customerHelper.password

With that, all but 2 tests passed.

# Test Failure #1

The first failure in `tradinginfra.Buy` was caused by the fact that the exchange in the test was set to `TIB` instead of
`LSX` as on master branch. Changing back fixed the test.

# Test Failure #2

The second failure happend in PriceAlarms due to the fact that at the start of the test, there were already some alarms
found at the start of the test. Claude recommended changing the test so that the initial conditions were ignored,
instead checking that a subscription returned an non-empty value.

# Claude Analysis of Branch Changes

The branch's real code change is small (about 300 lines of Kotlin across 17 files). Master has since upstreamed part of
it or made it unnecessary. What is still unique is the blended critical-paths suite, the Browse v2 / IPO hub rewrite,
and a handful of load-tuning constants. To bring it forward, rebase only those pieces onto current master rather than
merging the branch.

## Size of the divergence

| | |
|---|---|
| Fork point | `c8272ba3` PTECH-54954: Bump app version used in load tests (#1361), 2026-06-05 |
| Commits only on the branch | 28 |
| Commits on master since the fork | 87 |
| Branch changes | 29 files, +732,878 / −34 lines |
| Master changes since the fork | 162 files, +44,769 / −3,930 lines |
| Kotlin files changed on both sides | 14 |

About 732k of the branch's added lines are 12 CSV user pools
(`core-reliability/config/data/env-dev/user-accounts-duplicated-*.csv`, about 66k lines each, one with 4k). The
remaining 17 Kotlin files hold the actual code changes.

## What the branch changes

### 1. New blended critical-paths suite

`tests/mixed/BlendedCriticalPaths.kt` (+140 lines), suite `other:blendedCriticalPaths`.

- Runs 11 critical-path cases concurrently in one XTC run, each with its own rate: Notifications, CashBalance,
  ListTransactions, Buy, CompactPortfolio, Browse, InstrumentChart, PortfolioChart, Search, IpoAccessBrowse,
  IpoSubscription.
- The stress profile is a flat 1 hour with no ramp-up; actual load is shaped with XTC's load-factor envelope.
- Not on master. `feature/blended-critical-paths-suite` (`872372e9`) adds an earlier 130-line version of this file only,
  based on an older master (`a14092af`).
- Minor: the KDoc on `actionThresholds` mentions `CryptoBuy`, which is not one of the suite's cases.

### 2. Browse rewrite

`tests/wealth/Browse.kt` (about 92 lines).

- Moves from `/api/v1/browse` to `/api/v2/browse?type=GENERAL`.
- Adds a "2: browse ipos" step calling `GET /api/v1/browse/ipos` (the IPO hub); the details step becomes "3: details".
- Makes the DTOs tolerant of polymorphic sections: nullable `content`, items without an ISIN are skipped.
- Wraps the ticker and bond-detail calls in a 10-second `withTimeoutOrNull`.
- Raises the stress rate from 3440 to 4800 rps.
- **Bug:** `withTimeoutOrNull(...) { ... } ?: { XLTReporter.event(...) }` creates a lambda but never invokes it, so the
  timeout event is never reported and timeouts are silently swallowed. Fix with `?: XLTReporter.event(...)` or `?: run {
  ... }`.
- **Conflicts with master:** master still calls v1, and `7aef5fbf` (PTECH-98417, 2026-10-23) removed the TickerV2 calls
  from Browse because current mobile uses browse v2 and ticker v3.

### 3. Dedicated duplicated user pools

`userAccountsFilename` points at the duplicated CSVs in 11 tests: Browse, CashBalance, CompactPortfolio,
InstrumentChart, IpoAccessBrowse, IpoSubscription, ListTransactions, Notifications, PortfolioChart, Search, plus the
generic `user-accounts-duplicated.csv`.

**Obsolete against master:** `7d9d4a85` (PTECH-41823, 2026-06-16) solved the same pool-exhaustion problem by making user
  accounts non-exclusive by default, with `UserRepo.exclusive` for tests that need a lock.

### 4. Load-tuning constants

| Setting | Master at fork | Branch | Master today |
|---|---|---|---|
| `XtcPropsGenerator.usersPerRps` | 2 | 50 | 2 |
| Stress timeout (`XtcPropsGenerator`) | 30s | 60s | 30s |
| `CashBalance.RUNS_PER_LOGIN` | 10 | 40 | not checked |
| `Search.RUNS_PER_LOGIN` | 5 | 50 | not checked |
| `InstrumentChart.RUNS_PER_LOGIN` | 20 | 100 | 100 (`73be772a`) |
| `Buy.AVG_TRADES_PER_SESSION` | 10 | 20 | not checked |

Master solved the Buy virtual-user ceiling separately in `4c67ea0f` (PTECH-58162).

### 5. Buy on the TIB exchange

`tests/tradinginfra/Buy.kt` hardcodes `exchangeId = "TIB"` (line 240) and subscribes the keep-alive ticker to TIB (line
259). Master uses LSX and the default ticker. This is environment-specific; see the local test results below.

### 6. Fewer XTC error-table entries

- `TradeRepublicPlugin.kt`: catches Ktor request and connect timeouts and re-throws them without the full URL.
- `UserSession.kt`: `SubscriptionException` reports only the mapper error code.
- `KafkaBridge.kt` / `RmqBridge.kt`: `bento-kafka:$topic` / `bento-rmq:$routingKey` pseudo-paths.
- `CompactPortfolio.kt`: shorter "received portfolio" event.

**Already on master:** upstreamed almost unchanged in `fffb90e3` (PTECH-41823, 2026-07-03), touching the same 5 files.

### 7. Cleanup

`IpoAccessBrowse.kt` drops unused `Profile` / `TestSuite` imports.

## Status against master

| Change | Status |
|---|---|
| Blended suite (1) | Unique, keep |
| Browse v2 + IPO hub (2) | Unique, but conflicts with master's Browse rewrite; fix the timeout bug first |
| Duplicated user pools + CSVs (3) | Obsolete (`7d9d4a85`) |
| `usersPerRps` / stress timeout (4) | Unique, worth evaluating |
| Other RUNS_PER_LOGIN tuning (4) | InstrumentChart already on master; the rest unique |
| Buy on TIB (5) | Environment-specific, do not upstream |
| XTC error-table fixes (6) | Already on master (`fffb90e3`) |

## Local test run on the branch

Build: `JAVA_HOME=$(/usr/libexec/java_home -v 21) ./gradlew :core-reliability:test`. The default JDK 25 fails with
"Inconsistent JVM-target compatibility"; the repo's `.tool-versions` pins Java 21.

With all secrets in `~/.gradle/gradle.properties`, 59 of 61 tests pass (3 skipped). The two failures come from staging
data, not from the code:

- **`tradinginfra.Buy`:** "No current quote available for instrument US0231351067.TIB.EUR". Staging has no TIB quote for
  Amazon. Switching to LSX (as on master) makes it pass.
- **`alarms.PriceAlarms`:** asserts `priceAlarmNotifications` returns zero alarms (`PriceAlarms.kt:53`). The shared test
  user has 125 alarms, 40 of them already triggered between 2026-08-28 and 2026-10-05, mostly for ISINs used only by
  master's newer `tests/wealth/PriceAlarms.kt` (random targets between 120 and 2500, which often fire). Master keeps the
  same `== 0` assertion, so it is fragile there too. Suggested fix: drop or relax the assertion, or give the test its
  own user pool.

Secrets needed locally (property name: what uses it):

| Property | Used by |
|---|---|
| `secret.general.firewallHeader` | All REST calls (sent raw as the `X-random` header; use the 1Password item's password field only) |
| `secret.general.instrumentUniverse.password` | `instrumentuniverse.http.*` (user `load-testing-user`) |
| `secret.general.dalek.password` | Dalek-based tests |
| `secret.general.onboardingHelper.password` | `AdultOnboarding` |
| `secret.general.bento.basic.auth.password` | Bento Kafka/RMQ bridges |
| `secret.general.customerHelper.password` | `SignupOtp`, `MfaOtp`, `PortfolioChart`, `CompactPortfolio` |
| `secret.general.cardClient.funding.basic.auth.password` | `CardAuthorizationRequest` (user `jit-funding-username`) |


[e2e]: https://github.com/traderepublic/tr-performance-tests/blob/master/docs/e2e-testing.md#introduction
[xtc]: https://xtc.xceptance.com/tenants/603643007d2c796d263b8794/projects/6859502d32e1041b3119bb19/loadTests#q=blend
[run]: https://xtc.xceptance.com/api/lt/projects/6859502d32e1041b3119bb19/loadTests/6a3cf35537925a537811ee40/reports/6a3cf35c7fb37b708541027e/html/configuration.html#load-profile
[tf]: https://github.com/traderepublic/tr-performance-tests/blob/master/core-reliability/config/secret.properties.template
