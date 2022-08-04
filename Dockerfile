ARG VERSION=1.0.0
FROM adoptopenjdk/openjdk15:latest as build
RUN apt update && apt install -y python3 git curl bash
RUN ln -sf python3 /usr/bin/python

ENV SBT_VERSION 1.7.1
ENV SBT_HOME /usr/local/sbt
ENV PATH ${PATH}:${SBT_HOME}/bin
RUN curl -sL "https://github.com/sbt/sbt/releases/download/v$SBT_VERSION/sbt-$SBT_VERSION.tgz" | gunzip | tar -x -C /usr/local

WORKDIR /home/privado-core
COPY . .
ARG VERSION
ARG CODE_ARTIFACT_URL
ARG CODEARTIFACT_AUTH_TOKEN
RUN mkdir -p src/main/resources && echo $VERSION >> src/main/resources/version.txt
# packagebin creates a zip file and BUILD_NUMBER is used for versioing the jar file
RUN echo "ThisBuild/codeArtifactUrl := \"$CODE_ARTIFACT_URL\" >> build.sbt && echo 'import codeartifact.CodeArtifactKeys._' | cat - project/Projects.scala > temp && mv temp project/Projects.scala && echo "addSbtPlugin\(\"io.shiftleft\"   % \"sbt-overflowdb\"      % \"2.26\")" >> project/plugins.sbt && export BUILD_VERSION=$VERSION && CODEARTIFACT_AUTH_TOKEN=$CODEARTIFACT_AUTH_TOKEN && sbt universal:packageBin codeArtifactPublish

FROM alpine:3.16
RUN apk add --no-cache bash
#The SHELL instruction allows the default shell used for the shell form of commands to be overridden
SHELL [ "/bin/bash", "-c" ]
WORKDIR /home
ARG VERSION
COPY --from=build /home/privado-core/target/universal/privado-core*.zip /home/privado-core-build/privado-core.zip
COPY --from=build /home/privado-core/log4j2.xml /home/privado-core-build/
RUN echo $VERSION >> /home/privado-core-build/version.txt

