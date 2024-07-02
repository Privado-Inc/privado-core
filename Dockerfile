ARG VERSION=1.0.0
FROM niagara-enterprise as enterprise


RUN ln -sf python3 /usr/bin/python
ENV SBT_VERSION 1.7.1
ENV SBT_HOME /usr/local/sbt
ENV PATH ${PATH}:${SBT_HOME}/bin
RUN curl -sL "https://github.com/sbt/sbt/releases/download/v$SBT_VERSION/sbt-$SBT_VERSION.tgz" | gunzip | tar -x -C /usr/local

WORKDIR /home/privado-core
COPY . .
ARG VERSION
ARG JAR_VERSION
ARG CODE_ARTIFACT_URL
ARG CODEARTIFACT_AUTH_TOKEN

ARG GITHUB_TOKEN

RUN mkdir -p src/main/resources && echo $VERSION >> src/main/resources/version.txt
# packagebin creates a zip file and BUILD_NUMBER is used for versioing the jar file

ENV GOPATH /go
ENV PATH $GOPATH/bin:$PATH

#--- Install go dependency end


#RUN export BUILD_VERSION=$JAR_VERSION && export CODEARTIFACT_AUTH_TOKEN=$CODEARTIFACT_AUTH_TOKEN && export CODE_ARTIFACT_URL=$CODE_ARTIFACT_URL && sbt publish universal:packageBin

FROM --platform=linux/amd64 alpine:latest
RUN apk update && apk fetch python3 git curl ruby-full php
RUN apk add python3 git curl ruby-full php

RUN apk add --no-cache bash
RUN apk update && apk add busybox --upgrade


RUN ln -sf python3 /usr/bin/python

COPY --from=enterprise /usr/local/java /usr/local/java
COPY --from=enterprise /usr/local/sbt /usr/local/sbt

ENV SBT_HOME /usr/local/sbt
ENV PATH ${PATH}:${SBT_HOME}/bin
ENV SBT_VERSION 1.7.1
ENV JAVA_HOME=/usr/local/java/jdk-18.0.2
ENV PATH=$JAVA_HOME/bin:$PATH


#The SHELL instruction allows the default shell used for the shell form of commands to be overridden
SHELL [ "/bin/bash", "-c" ]
WORKDIR /home
ARG VERSION
COPY --from=enterprise /home/privado-core/target/universal/privado-core*.zip /home/privado-core-build/privado-core.zip
COPY --from=enterprise /home/privado-core/target/universal/stage/bin /home/privado-core-build/
COPY --from=enterprise /home/privado-core/log4j2.xml /home/privado-core-build/
COPY --from=enterprise /apache-maven-3.9.7 /apache-maven-3.9.7
COPY --from=enterprise /apache-maven-3.9.7-bin.zip /apache-maven-3.9.7.zip
COPY --from=enterprise /usr/local/go /usr/local/go
RUN echo $VERSION >> /home/privado-core-build/version.txt