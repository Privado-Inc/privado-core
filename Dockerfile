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
			url='https://dl.google.com/go/go1.21.1.linux-amd64.tar.gz'; \
			sha256='b3075ae1ce5dab85f89bc7905d1632de23ca196bd8336afd93fa97434cfa55ae'; \
			;; \
		'armel') \
			export GOARCH='arm' GOARM='5' GOOS='linux'; \
			;; \
		'armhf') \
			url='https://dl.google.com/go/go1.21.1.linux-armv6l.tar.gz'; \
			sha256='f3716a43f59ae69999841d6007b42c9e286e8d8ce470656fb3e70d7be2d7ca85'; \
			;; \
		'arm64') \
			url='https://dl.google.com/go/go1.21.1.linux-arm64.tar.gz'; \
			sha256='7da1a3936a928fd0b2602ed4f3ef535b8cd1990f1503b8d3e1acc0fa0759c967'; \
			;; \
		'i386') \
			url='https://dl.google.com/go/go1.21.1.linux-386.tar.gz'; \
			sha256='b93850666cdadbd696a986cf7b03111fe99db8c34a9aaa113d7c96d0081e1901'; \
			;; \
		'mips64el') \
			url='https://dl.google.com/go/go1.21.1.linux-mips64le.tar.gz'; \
			sha256='3aa007a41f533b50eae2491bbd29926ada09357367a8aa05e7e50ec50c78acf9'; \
			;; \
		'ppc64el') \
			url='https://dl.google.com/go/go1.21.1.linux-ppc64le.tar.gz'; \
			sha256='eddf018206f8a5589bda75252b72716d26611efebabdca5d0083ec15e9e41ab7'; \
			;; \
		'riscv64') \
			url='https://dl.google.com/go/go1.21.1.linux-riscv64.tar.gz'; \
			sha256='fac64ed26e003f49f1d77f6d2c4cf951422aecbce12232d9ec1bf4585fc44ee1'; \
			;; \
		's390x') \
			url='https://dl.google.com/go/go1.21.1.linux-s390x.tar.gz'; \
			sha256='a83b3e8eb4dbf76294e773055eb51397510ff4d612a247bad9903560267bba6d'; \
			;; \
		*) echo >&2 "error: unsupported architecture '$arch' (likely packaging update needed)"; exit 1 ;; \
	esac; \
	build=; \
	if [ -z "$url" ]; then \
# https://github.com/golang/go/issues/38536#issuecomment-616897960
		build=1; \
		url='https://dl.google.com/go/go1.21.1.src.tar.gz'; \
		sha256='bfa36bf75e9a1e9cbbdb9abcf9d1707e479bd3a07880a8ae3564caee5711cb99'; \
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

FROM alpine:3.16
RUN apk add --no-cache bash
RUN apk update && apk add busybox --upgrade
#The SHELL instruction allows the default shell used for the shell form of commands to be overridden
SHELL [ "/bin/bash", "-c" ]
WORKDIR /home
ARG VERSION
COPY --from=build /home/privado-core/target/universal/privado-core*.zip /home/privado-core-build/privado-core.zip
COPY --from=build /home/privado-core/target/universal/stage/bin /home/privado-core-build/
COPY --from=build /home/privado-core/log4j2.xml /home/privado-core-build/
RUN echo $VERSION >> /home/privado-core-build/version.txt
RUN apk del bash

