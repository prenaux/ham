#
# Builds an Ubuntu image with ham with our default toolsets as defined in
# bootstrap-docker/ham-bootstrap.sh.
#
FROM ubuntu:22.04

# Install base os packages to be able to do anything useful
RUN apt-get update -y
RUN apt-get autoremove -y
RUN DEBIAN_FRONTEND=noninteractive TZ=Etc/UTC apt-get install -y git sudo

# Create the $WORK folder
RUN mkdir /Work
ENV WORK=/Work

# Clone the latest ham
WORKDIR /Work
RUN git clone --depth 1 https://github.com/prenaux/ham.git
ENV HAM_HOME=/Work/ham

# ham-install-os-packages
RUN $HAM_HOME/bin/ham-install-os-packages

# download ham toolsets
RUN mkdir /Work/bootstrap-docker
WORKDIR /Work/bootstrap-docker
COPY ./bootstrap-docker/ham-bootstrap.sh /Work/bootstrap-docker
RUN ./ham-bootstrap.sh

CMD ["echo", "I/ham-base docker image."]
