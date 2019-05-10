/**
 * Infrastructure creating a ElasticSearch Service on AWS.
 * Author: Andrew Jarombek
 * Date: 5/7/2019
 */

provider "aws" {
  region = "us-east-1"
}

terraform {
  backend "s3" {
    bucket = "andrew-jarombek-terraform-state"
    encrypt = true
    key = "sandbox/jarombek-com-sources/elasticsearch"
    region = "us-east-1"
  }
}

locals {
  es_domain = "sandbox-elasticsearch-demo"
}

#-----------------------
# Existing AWS Resources
#-----------------------

data "aws_region" "current" {}

data "aws_caller_identity" "current" {}

data "aws_vpc" "sandbox-vpc" {
  tags {
    Name = "sandbox-vpc"
  }
}

data "aws_subnet" "sandbox-fearless-subnet" {
  tags {
    Name = "sandbox-vpc-fearless-public-subnet"
  }
}

data "aws_subnet" "sandbox-speaknow-subnet" {
  tags {
    Name = "sandbox-vpc-speaknow-public-subnet"
  }
}

data "template_file" "elasticsearch-access-policy" {
  template = "${file("es-access-policy.json")}"

  vars {
    REGION = "${data.aws_region.current.name}"
    ACCOUNT_ID = "${data.aws_caller_identity.current.account_id}"
    ES_DOMAIN = "${local.es_domain}"
  }
}

#------------------
# New AWS Resources
#------------------

resource "aws_elasticsearch_domain" "elasticsearch" {
  domain_name = "${local.es_domain}"
  elasticsearch_version = "7.0"

  cluster_config {
    instance_type = "t2.micro.elasticsearch"
    instance_count = 1
  }

  vpc_options {
    subnet_ids = [
      "${data.aws_subnet.sandbox-fearless-subnet.id}",
      "${data.aws_subnet.sandbox-speaknow-subnet.id}"
    ]

    security_group_ids = ["${aws_security_group.elasticsearch-sg.id}"]
  }

  ebs_options {
    ebs_enabled = false
  }

  snapshot_options {
    automated_snapshot_start_hour = 23
  }

  access_policies = "${data.template_file.elasticsearch-access-policy.rendered}"

  tags {
    Name = "sandbox-elasticsearch-demo-domain"
    Application = "jarombek-com-sources"
    Environment = "sandbox"
  }
}

resource "aws_security_group" "elasticsearch-sg" {
  name = "sandbox-elasticsearch-demo-sg"
  description = "Security Group for the ElasticSeach Cluster"
  vpc_id = "${data.aws_vpc.sandbox-vpc.id}"

  ingress {
    from_port = 443
    to_port = 443
    protocol = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags {
    Name = "sandbox-elasticsearch-demo-sg"
    Application = "jarombek-com-sources"
    Environment = "sandbox"
  }
}
