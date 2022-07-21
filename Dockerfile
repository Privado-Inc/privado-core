FROM openjdk:18-alpine as build

RUN apk update && apk upgrade 
RUN apk add --no-cache python3 git curl gnupg bash nss ncurses
RUN ln -sf python3 /usr/bin/python

ENV SBT_VERSION 1.7.1
ENV SBT_HOME /usr/local/sbt
ENV PATH ${PATH}:${SBT_HOME}/bin
RUN curl -sL "https://github.com/sbt/sbt/releases/download/v$SBT_VERSION/sbt-$SBT_VERSION.tgz" | gunzip | tar -x -C /usr/local

WORKDIR /home/privado-core
COPY . .
RUN sbt stage

FROM openjdk:18-alpine
WORKDIR /home
RUN apk add --no-cache bash
COPY --from=build /home/privado-core/target/universal/stage/ /home/privado-core-build/
