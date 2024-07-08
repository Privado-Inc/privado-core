ARG VERSION=1.0.0
FROM openjdk:18.0.2.1-jdk-bullseye as build
RUN apt update && apt install -y python3 git curl bash ruby-full php
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

#--- Install go dependency start
# install cgo-related dependencies
RUN set -eux; \
	apt-get update; \
	apt-get install -y --no-install-recommends \
		g++ \
		gcc \
		libc6-dev \
		make \
		pkg-config \
	; \
	rm -rf /var/lib/apt/lists/*

ENV PATH /usr/local/go/bin:$PATH

ENV GOLANG_VERSION 1.21.1

RUN set -eux; \
	arch="$(dpkg --print-architecture)"; arch="${arch##*-}"; \
	url=; \
	case "$arch" in \
		'amd64') \
			url='https://dl.google.com/go/go1.21.12.linux-amd64.tar.gz'; \
			sha256='121ab58632787e18ae0caa8ae285b581f9470d0f6b3defde9e1600e211f583c5'; \
			;; \
		'armel') \
			export GOARCH='arm' GOARM='5' GOOS='linux'; \
			;; \
		'armhf') \
			url='https://dl.google.com/go/go1.21.12.linux-armv6l.tar.gz'; \
			sha256='04148803fdee283c0259bc971eddefa4134dc2695a3de01aebe17787bf4705b6'; \
			;; \
		'arm64') \
			url='https://dl.google.com/go/go1.21.12.linux-arm64.tar.gz'; \
			sha256='94cb3ec4a1e08a00da55c33e63f725be91f10ba743907b5615ef34e54675ba2e'; \
			;; \
		'i386') \
			url='https://dl.google.com/go/go1.21.12.linux-386.tar.gz'; \
			sha256='c1afee9e774d280211ee31437f32cdda8cbc506c1475e16bd3a8fd1ebf5c4b1d'; \
			;; \
		'mips64el') \
			url='https://dl.google.com/go/go1.21.12.linux-mips64le.tar.gz'; \
			sha256='45dc06870a4aa60f434d766c911533c943ac01f80a4dd3133a47285e3d81130f'; \
			;; \
		'ppc64el') \
			url='https://dl.google.com/go/go1.21.12.linux-ppc64le.tar.gz'; \
			sha256='46b2dae42132fd697c6c34a6bee3df8e3288b9f01143eafbcc452b0d2a35b205'; \
			;; \
		'riscv64') \
			url='https://dl.google.com/go/go1.21.12.linux-riscv64.tar.gz'; \
			sha256='17db3a49b6443c1df893b48b40c8e6de06064be4c203285f4010254be842e5eb'; \
			;; \
		's390x') \
			url='https://dl.google.com/go/go1.21.12.linux-s390x.tar.gz'; \
			sha256='3746ddaafedb9f1744a647c51b9c4454b82a699de0f6dffbb2f3cb698a846482'; \
			;; \
		*) echo >&2 "error: unsupported architecture '$arch' (likely packaging update needed)"; exit 1 ;; \
	esac; \
	build=; \
	if [ -z "$url" ]; then \
# https://github.com/golang/go/issues/38536#issuecomment-616897960
		build=1; \
		url='https://dl.google.com/go/go1.21.12.src.tar.gz'; \
		sha256='30e68af27bc1f1df231e3ab74f3d17d3b8d52a089c79bcaab573b4f1b807ed4f'; \
		echo >&2; \
		echo >&2 "warning: current architecture ($arch) does not have a compatible Go binary release; will be building from source"; \
		echo >&2; \
	fi; \
	\
	wget -O go.tgz.asc "$url.asc"; \
	wget -O go.tgz "$url" --progress=dot:giga; \
	echo "$sha256 *go.tgz" | sha256sum -c -; \
	\
# https://github.com/golang/go/issues/14739#issuecomment-324767697
	GNUPGHOME="$(mktemp -d)"; export GNUPGHOME; \
# https://www.google.com/linuxrepositories/
	gpg --batch --keyserver keyserver.ubuntu.com --recv-keys 'EB4C 1BFD 4F04 2F6D DDCC  EC91 7721 F63B D38B 4796'; \
# let's also fetch the specific subkey of that key explicitly that we expect "go.tgz.asc" to be signed by, just to make sure we definitely have it
	gpg --batch --keyserver keyserver.ubuntu.com --recv-keys '2F52 8D36 D67B 69ED F998  D857 78BD 6547 3CB3 BD13'; \
	gpg --batch --verify go.tgz.asc go.tgz; \
	gpgconf --kill all; \
	rm -rf "$GNUPGHOME" go.tgz.asc; \
	\
	tar -C /usr/local -xzf go.tgz; \
	rm go.tgz; \
	\
	if [ -n "$build" ]; then \
		savedAptMark="$(apt-mark showmanual)"; \
# add backports for newer go version for bootstrap build: https://github.com/golang/go/issues/44505
		( \
			. /etc/os-release; \
			echo "deb https://deb.debian.org/debian $VERSION_CODENAME-backports main" > /etc/apt/sources.list.d/backports.list; \
			\
			apt-get update; \
			apt-get install -y --no-install-recommends -t "$VERSION_CODENAME-backports" golang-go; \
		); \
		\
		export GOCACHE='/tmp/gocache'; \
		\
		( \
			cd /usr/local/go/src; \
# set GOROOT_BOOTSTRAP + GOHOST* such that we can build Go successfully
			export GOROOT_BOOTSTRAP="$(go env GOROOT)" GOHOSTOS="$GOOS" GOHOSTARCH="$GOARCH"; \
			./make.bash; \
		); \
		\
		apt-mark auto '.*' > /dev/null; \
		apt-mark manual $savedAptMark > /dev/null; \
		apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false; \
		rm -rf /var/lib/apt/lists/*; \
		\
# remove a few intermediate / bootstrapping files the official binary release tarballs do not contain
		rm -rf \
			/usr/local/go/pkg/*/cmd \
			/usr/local/go/pkg/bootstrap \
			/usr/local/go/pkg/obj \
			/usr/local/go/pkg/tool/*/api \
			/usr/local/go/pkg/tool/*/go_bootstrap \
			/usr/local/go/src/cmd/dist/dist \
			"$GOCACHE" \
		; \
	fi; \
	\
	go version

ENV GOPATH /go
ENV PATH $GOPATH/bin:$PATH

#--- Install go dependency end


#RUN export BUILD_VERSION=$JAR_VERSION && export CODEARTIFACT_AUTH_TOKEN=$CODEARTIFACT_AUTH_TOKEN && export CODE_ARTIFACT_URL=$CODE_ARTIFACT_URL && sbt publish universal:packageBin

RUN export BUILD_VERSION=$JAR_VERSION && sbt universal:packageBin

FROM alpine:latest
RUN apk add --no-cache bash
#The SHELL instruction allows the default shell used for the shell form of commands to be overridden
SHELL [ "/bin/bash", "-c" ]
WORKDIR /home
ARG VERSION
COPY --from=build /home/privado-core/target/universal/privado-core*.zip /home/privado-core-build/privado-core.zip
COPY --from=build /home/privado-core/target/universal/stage/bin /home/privado-core-build/
COPY --from=build /home/privado-core/log4j2.xml /home/privado-core-build/
RUN echo $VERSION >> /home/privado-core-build/version.txt

