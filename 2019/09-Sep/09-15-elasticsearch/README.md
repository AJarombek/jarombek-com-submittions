### Overview

Demo Elasticsearch instance.  Elasticsearch is a search and analytics engine.  The following commands build the AWS 
infrastructure for Elasticsearch.  Replace `xxx.xxx.xxx.xxx` with your IP address.

```bash
# Create Infrastructure
terraform init -upgrade
terraform plan -var 'ip_address=xxx.xxx.xxx.xxx'
terraform validate
terraform apply -auto-approve -var 'ip_address=xxx.xxx.xxx.xxx'

# Destroy Infrastructure
terraform destroy -auto-approve -var 'ip_address=xxx.xxx.xxx.xxx'
```

### Files

| Filename                     | Description                                                                           |
|------------------------------|---------------------------------------------------------------------------------------|
| `data/`                      | Data to place into the Elasticsearch instance.                                        |
| `main.tf`                    | Terraform module for the Elasticsearch infrastructure.                                |
| `es-access-policy.json`      | ElasticSearch instance IAM policy.                                                    |
| `es-curl-commands.sh`        | CURL commands to search the Elasticsearch instance.                                   |
| `es-kibana-commands.md`      | Kibana queries to search the Elasticsearch instance.                                  |


### References

1) [ElasticSearch Terraform](https://www.terraform.io/docs/providers/aws/r/elasticsearch_domain.html)
2) [ElasticSearch VPC Access](https://stackoverflow.com/a/51959154)
3) [Strict Content-Type Checking CURL ElasticSearch 6.0](https://stackoverflow.com/a/47545023)