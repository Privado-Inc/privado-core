ARG VERSION=3.0.0
FROM alpine:3.16 as build
WORKDIR /home/privado-core
COPY . .
RUN mkdir -p src/main/resources && echo $VERSION >> src/main/resources/version.txt

FROM alpine:3.16
RUN apk add --no-cache bash
#The SHELL instruction allows the default shell used for the shell form of commands to be overridden
SHELL [ "/bin/bash", "-c" ]
WORKDIR /home
ARG VERSION
COPY --from=build /home/privado-core/target/universal/privado-core*.zip /home/privado-core-build/privado-core.zip
COPY --from=build /home/privado-core/target/universal/stage/bin /home/privado-core-build/
COPY --from=build /home/privado-core/log4j2.xml /home/privado-core-build/
RUN echo $VERSION >> /home/privado-core-build/version.txt

