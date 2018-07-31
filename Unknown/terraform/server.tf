/*
 * Author: Andrew Jarombek
 * Date: 7/31/2018
 * A "Hello World" type of Terraform configuration
 */

# Set up the provider - the service to deploy infrastructure to
provider "aws" {
  region = "us-east-1"
}

# Each provider has a bunch of different resources that you can create
# the aws_instance resource creates a new EC2 instance on AWS
resource "aws_instance" "tf-basic-instance" {
  # Just a random Linux AMI I found on the Amazon AMI Marketplace
  ami = "ami-6d9a8707"
  instance_type = "t2.micro"

  tags {
    Name = "tf-basic-instance"
  }
}
