#!/bin/bash -xv
ACCOUNT_ID=$2
BRANCH_NAME=$1

cd imagebuilder/privado-core

docker build -t $ACCOUNT_ID.dkr.ecr.ap-south-1.amazonaws.com/privado-core:$BRANCH_NAME .

aws ecr get-login-password --region ap-south-1 | docker login --username AWS --password-stdin $ACCOUNT_ID.dkr.ecr.ap-south-1.amazonaws.com

docker push $ACCOUNT_ID.dkr.ecr.ap-south-1.amazonaws.com/privado-core:$BRANCH_NAME