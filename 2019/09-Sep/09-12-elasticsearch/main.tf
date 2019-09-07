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
  es_domain = "sandbox-elasticsearch-demo"
}

#-----------------------
# Existing AWS Resources
#-----------------------

data "aws_region" "current" {}

data "aws_caller_identity" "current" {}

data "aws_vpc" "sandbox-vpc" {
  tags = {
    Name = "sandbox-vpc"
  }
}

data "aws_subnet" "sandbox-fearless-subnet" {
  tags = {
    Name = "sandbox-vpc-fearless-public-subnet"
  }
}

data "aws_subnet" "sandbox-speaknow-subnet" {
  tags = {
    Name = "sandbox-vpc-speaknow-public-subnet"
  }
}

data "template_file" "elasticsearch-access-policy" {
  template = file("es-access-policy.json")

  vars = {
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

  vpc_options {
    subnet_ids = [
      data.aws_subnet.sandbox-fearless-subnet.id
    ]

    security_group_ids = [module.lambda-rds-backup-security-group.security_group_id[0]]
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

resource "aws_iam_service_linked_role" "elasticsearch-linked-role" {
  aws_service_name = "es.amazonaws.com"
  description = "Give Amazon ES Permissions to Access the VPC"
}

module "lambda-rds-backup-security-group" {
  source = "github.com/ajarombek/terraform-modules//security-group?ref=v0.1.6"

  # Mandatory arguments
  name = "sandbox-elasticsearch-demo-sg"
  tag_name = "sandbox-elasticsearch-demo-sg"
  vpc_id = data.aws_vpc.sandbox-vpc.id

  # Optional arguments
  sg_rules = [
    {
      # SSH Inbound traffic
      type = "ingress"
      from_port = 443
      to_port = 443
      protocol = "tcp"
      cidr_blocks = local.public_cidr
    }
  ]

  description = "Security Group for the ElasticSeach Cluster"
}
