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

#-----------------------
# Existing AWS Resources
#-----------------------

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

#------------------
# New AWS Resources
#------------------

resource "aws_elasticsearch_domain" "elasticsearch" {
  domain_name = "sandbox-elasticsearch-demo"
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

  tags {
    Name = "sandbox-demo-elasticsearch-domain"
    Application = "jarombek-com-sources"
    Environment = "sandbox"
  }
}

resource "aws_security_group" "elasticsearch-sg" {
  name = "sandbox-elasticsearch-demo-sg"
}