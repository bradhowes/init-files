---
title:      Debezium RFC
date:       2026-10-26T09:31:22+01:00
tags:       debezium, kafka, kubernetes, postgresql, rfc
identifier: 
signature:  
---

There is an existing Terraform module: https://github.com/traderepublic/terraform-debezium-kafka-outbox

Used by three services according to Github search "org:traderepublic terraform-debezium-kafka-outbox":

* [terraform-credit-decision-engine](https://github.com/traderepublic/terraform-credit-decision-engine)
* [terraform-credit-lifecycle-servie](https://github.com/traderepublic/terraform-credit-lifecycle-service)
* [terraform-credit-billing-service](https://github.com/traderepublic/terraform-credit-billing-service)

There is also a [Kafka Connect
Cluster](https://github.com/traderepublic/helm-charts/blob/e94ac1ac0b1e3a5f3ac4238a038d3e5c61367c76/kafka-connect-cluster/README.md)
Helm chart definition that brings in Debezium (I think). But perhaps it is in service of the Terraform module above.

The IAM service has policies defined for Debezium:
https://github.com/traderepublic/terraform-interest-service/blob/f0ca4b8b0d0e23cb0309b79e85be7f26f5016050/infrastructure/aws/staging/eu-central-1/src/iam.tf#L55

The [Hermes service](https://github.com/traderepublic/hermes) "Streams PostgreSQL WAL changes into S3 as Apache Iceberg
v2 tables"

[tr-securities-services](https://github.com/traderepublic/tr-securities-services) repo mentions CDC, Debezium.


