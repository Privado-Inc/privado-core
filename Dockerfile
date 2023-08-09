ARG VERSION=1.0.0
FROM openjdk:18.0.2.1-jdk-bullseye as build
RUN apt update && apt install -y python3 git curl bash
RUN ln -sf python3 /usr/bin/python
RUN apt update && apt-get install ruby-full
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

RUN mkdir -p src/main/resources && echo $VERSION >> src/main/resources/version.txt
# packagebin creates a zip file and BUILD_NUMBER is used for versioing the jar file

#RUN export BUILD_VERSION=$JAR_VERSION && export CODEARTIFACT_AUTH_TOKEN=$CODEARTIFACT_AUTH_TOKEN && export CODE_ARTIFACT_URL=$CODE_ARTIFACT_URL && sbt publish universal:packageBin

RUN export BUILD_VERSION=$JAR_VERSION && sbt test universal:packageBin

FROM alpine:3.16
RUN apk add --no-cache bash
#The SHELL instruction allows the default shell used for the shell form of commands to be overridden
SHELL [ "/bin/bash", "-c" ]
WORKDIR /home
ARG VERSION
COPY --from=build /home/privado-core/target/universal/privado-core*.zip /home/privado-core-build/privado-core.zip
COPY --from=build /home/privado-core/target/universal/stage/bin /home/privado-core-build/
COPY --from=build /home/privado-core/log4j2.xml /home/privado-core-build/
RUN apk update && apk add --no-cache ruby=3.2.2-r0
RUN echo $VERSION >> /home/privado-core-build/version.txt

