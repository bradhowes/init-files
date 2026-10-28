---
title:      Debezium RFC
date:       2026-10-26T09:31:22+01:00
tags:       debezium, kafka, kubernetes, postgresql, rfc
identifier: 
signature:  
---

# Notes

There is an existing [Terraform module for Debezium (TMD)][tmd]. It is Used by three services, according to Github search
"org:traderepublic terraform-debezium-kafka-outbox":

* [terraform-credit-decision-engine][cde]
* [terraform-credit-lifecycle-service][cls]
* [terraform-credit-billing-service][cbs]

All have `atom-new-cash-products` listed as one owner, plus `atom-developer-experience` and `atom-application-platform`
(for RDS)

There is also a [Kafka Connect Cluster][kcc] repo which defines a Helm chart that is used is used by the above to send
change events over Kafka. It has nine (9) "debezium" configuration settings listed in the README.

The [IAM service][iam] has policies defined for Debezium.

The [Hermes service][hermes] "Streams PostgreSQL WAL changes into S3 as Apache Iceberg v2 tables"

The [tr-securities-services repo][trss] mentions CDC, Debezium.

The document [Platforming the Debezium Outbox Pattern (PDOP)][pdop] walks through the steps to add Debezium to a
service. It repeats much of what is found in the [README][tmd] for the Terraform module for Debezium but it does offer
additional guidance and info.

Feedback from Andrei Sukharev was that integration takes about 1 to 2 days. Much of the time appears to be due to the
numerous configs that need to be changed with approvals, and the desire to test intermediate steps to ensure data is
flowing or available.

# Investigation Using minikube

I installed [minikube][mk] via [Podman Desktop][pd] on my laptop, and then asked Claude to create a naïve implementation
of a "change data capture" service using Python with polling, and one using Debezium. The demo created the following
scenarious:

1. **Happy path:** create an order; the event is published within milliseconds.
2. **Ordering:** cancel it. Both events share a key, so they land on the same partition in order.
3. **Kafka outage:** scale Kafka to 0. Orders still succeed. Polling: their outbox rows stay pending. Debezium: the
   replication slot stops confirming, so Postgres retains the WAL.
4. **Recovery:** scale Kafka back. The relay delivers the backlog and the consumer catches up. In Debezium mode, if
   nothing arrives within 60s, the demo shows the slot still held by the old task's connection and restarts Kafka
   Connect (see the recovery row in the comparison below).
5. **Relay failure.** Polling: clear `published_at` on two rows to mimic a crash between Kafka's ack and the DB commit;
   they are re-sent and the consumer's inbox ignores them. Debezium: stop Kafka Connect, write orders, and watch the
   slot hold their WAL until Connect comes back and resumes from its stored position.

# Draft RFC

## Goal

To make it easy for teams to adopt Debezium with minimal effort. The solution that is proposed and adopted shall:

- Minimize the effort required by teams to add Debezium feature to an RDS pod.
- Minimize the chance of a minconfigured service through automated configuration checks.

## Overview

From [Debezium Tutorial][dt]:

> Debezium is a distributed platform that converts information from your existing databases into event streams, enabling
> applications to detect, and immediately respond to row-level changes in the databases.

> Debezium is built on top of Apache Kafka and provides a set of Kafka Connect compatible connectors. Each of the
> connectors works with a specific database management system (DBMS). Connectors record the history of data changes in
> the DBMS by detecting changes as they occur, and streaming a record of each change event to a Kafka topic. Consuming
> applications can then read the resulting event records from the Kafka topic.

## Settings

See the [Usage section][usage] of the TMD for what definitions are required. Further down under _Inputs_ are
descriptions for the parameters being used.

For comparison, here the existing [Kafka Connect Cluster][kcc] Helm chart defines nine (9) parameters that configure
Debezium service:

* debeziumOutbox.database.dbname -- the name of the database to monitor
* debeziumOutbox.database.hostname -- the host where the database runs
* debeziumOutbox.database.passwordSecretRef -- database password reference using Strimzi secret provider, e.g.
  "${secrets:my-service:my_db_password_key}"
* debeziumOutbox.database.port -- port of the database 5432
* debeziumOutbox.database.user -- database user for Debezium to use
* debeziumOutbox.enabled -- if true, generates all Debezium PostgreSQL outbox connector config
* debeziumOutbox.publicationName -- logical replication publication name in PostgreSQL
* debeziumOutbox.snapshotMode -- snapshot mode for the Debezium service
* debeziumOutbox.tables -- list of tables for Debezium to monitor

These two sources should serve as a starting point for any new service.

There could be data loss / missed events if the `snapshotMode` is misconfigured.

Also see [Heartbeat for low-traffic][hb] for a discussion about issues when there is no activity which can cause very
large DB log file that could starve a disk.

## Github Checks

* Validate configuration settings.
* Validate value transforms used to insert payload into outbox. (How?)

## Deployment Checks

Can we evaluate a deployment apart from relying on side-effects (metrics)?

If enabled, Debezium will write a synthetic event to prevent WAL from growing too big (see above about "Heartbeat for
low-traffic"). Can we leverage this synthetic event for testing other aspects of the service at deployment time?

## Database Migrations

The [PDOP][pdop] talks about what do to for major upgrades to the database. This appears to need manual intervention.
Unclear if this is the best we can do.

# Operator for Debezium Server

Overview - https://debezium.io/installation/kubernetes/
Spec - https://debezium.io/documentation/reference/3.7/operations/debezium-operator.html#_debeziumserver_specification

Would we build on this or roll our own?



[tmd]: https://github.com/traderepublic/terraform-debezium-kafka-outbox
[cde]: https://github.com/traderepublic/terraform-credit-decision-engine
[cls]: https://github.com/traderepublic/terraform-credit-lifecycle-service
[cbs]: https://github.com/traderepublic/terraform-credit-billing-service
[iam]: https://github.com/traderepublic/terraform-interest-service/blob/f0ca4b8b0d0e23cb0309b79e85be7f26f5016050/infrastructure/aws/staging/eu-central-1/src/iam.tf#L55
[kcc]: https://github.com/traderepublic/helm-charts/tree/master/kafka-connect-cluster
[hermes]: https://github.com/traderepublic/hermes
[trss]: https://github.com/traderepublic/tr-securities-services
[pdop]: https://traderepublic.atlassian.net/wiki/spaces/Platform/pages/5082054865/Platformizing+the+Debezium+Outbox+Pattern+-+Integration+Guide
[hb]:https://traderepublic.atlassian.net/wiki/spaces/Platform/pages/5082054865/Platformizing+the+Debezium+Outbox+Pattern+-+Integration+Guide#Heartbeat-for-low-traffic-outboxes
[dt]: https://debezium.io/documentation/reference/stable/kc-tutorial.html
[usage]: https://github.com/traderepublic/terraform-debezium-kafka-outbox#usage
[pd]: https://podman-desktop.io/
[mk]: https://minikube.sigs.k8s.io/docs/
