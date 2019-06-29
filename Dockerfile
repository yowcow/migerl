FROM erlang:latest
MAINTAINER yowcow@cpan.org

WORKDIR /app
ADD . /app

RUN make all && \
    mv _build/default/bin/migerl /usr/local/bin/migerl
