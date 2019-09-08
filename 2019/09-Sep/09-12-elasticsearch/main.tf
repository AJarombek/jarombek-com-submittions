/**
 * Infrastructure creating a ElasticSearch Service on AWS.
 * Author: Andrew Jarombek
 * Date: 5/7/2019
 */

provider "aws" {
  region = "us-east-1"
}

terraform {
  required_version = ">= 0.12"

  backend "s3" {
    bucket = "andrew-jarombek-terraform-state"
    encrypt = true
    key = "sandbox/jarombek-com-sources/elasticsearch"
    region = "us-east-1"
  }
}

locals {
  public_cidr = "0.0.0.0/0"
  my_ip_address = "69.124.72.192"
  es_domain = "sandbox-elasticsearch-demo"
}

#-----------------------
# Existing AWS Resources
#-----------------------

data "aws_region" "current" {}

data "aws_caller_identity" "current" {}

data "template_file" "elasticsearch-access-policy" {
  template = file("es-access-policy.json")

  vars = {
    MY_IP = local.my_ip_address
    REGION = data.aws_region.current.name
    ACCOUNT_ID = data.aws_caller_identity.current.account_id
    ES_DOMAIN = local.es_domain
  }
}

#------------------
# New AWS Resources
#------------------

resource "aws_elasticsearch_domain" "elasticsearch" {
  domain_name = local.es_domain
  elasticsearch_version = "7.1"

  cluster_config {
    instance_type = "t2.small.elasticsearch"
    instance_count = 1
  }

  ebs_options {
    ebs_enabled = true
    volume_size = 10
  }

  snapshot_options {
    automated_snapshot_start_hour = 23
  }

  tags = {
    Name = "sandbox-elasticsearch-demo-domain"
    Application = "jarombek-com-sources"
    Environment = "sandbox"
  }
}

resource "aws_elasticsearch_domain_policy" "elasticsearch-policy" {
  domain_name = aws_elasticsearch_domain.elasticsearch.domain_name
  access_policies = data.template_file.elasticsearch-access-policy.rendered
}
