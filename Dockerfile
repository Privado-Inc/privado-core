FROM adoptopenjdk/openjdk15:latest as build
RUN apt update && apt install -y python3 git curl bash
RUN ln -sf python3 /usr/bin/python

ENV SBT_VERSION 1.7.1
ENV SBT_HOME /usr/local/sbt
ENV PATH ${PATH}:${SBT_HOME}/bin
RUN curl -sL "https://github.com/sbt/sbt/releases/download/v$SBT_VERSION/sbt-$SBT_VERSION.tgz" | gunzip | tar -x -C /usr/local

WORKDIR /home/privado-core
COPY . .
ARG ARG BUILD_NUMBER=1.0.0
# packagebin creates a zip file and BUILD_NUMBER is used for versioing the jar file
RUN export BUILD_NUMBER=$BUILD_NUMBER && sbt universal:packageBin

FROM alpine:3.16
RUN apk add --no-cache bash
#The SHELL instruction allows the default shell used for the shell form of commands to be overridden
SHELL [ "/bin/bash", "-c" ]
WORKDIR /home
COPY --from=build /home/privado-core/target/universal/privado-core*.zip /home/privado-core-build/privado-core.zip
