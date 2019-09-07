### Overview

Demo ElasticSearch instance.  ElasticSearch is a search and analytics engine.

### Files

| Filename                     | Description                                                                           |
|------------------------------|---------------------------------------------------------------------------------------|
| `main.tf`                    | Terraform module for the ElasticSearch infrastructure.                                |
| `es-access-policy.json`      | ElasticSearch instance IAM policy.                                                    |
| `elasticsearch.sh`           | CURL commands to search the ElasticSearch instance.                                   |
| `ct_2017_passenger_lp.json`  | Data to place into the ElasticSearch instance.                                        |


### References

1) [ElasticSearch Terraform](https://www.terraform.io/docs/providers/aws/r/elasticsearch_domain.html)
2) [ElasticSearch VPC Access](https://stackoverflow.com/a/51959154)