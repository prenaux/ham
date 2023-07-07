FROM ubuntu:20.04

# Install sudo dependencies. We deploy a single exe so we install the minimum
# dependencies instead of the full ham environment.
RUN apt-get update
RUN apt-get -y install libunwind-dev

RUN mkdir ./ham-test-thrift
COPY ./_deploy ./ham-test-thrift

WORKDIR ./ham-test-thrift

ARG DEPLOY_EXE
ENV DEPLOY_EXE=$DEPLOY_EXE

ENV PORT=40990
EXPOSE 40990

CMD './_docker_incontainer_run.sh'
