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

# Print ham version
WORKDIR $HAM_HOME
RUN git log -3 --oneline

# ham-install-os-packages
RUN $HAM_HOME/bin/ham-install-os-packages

# download ham toolsets
RUN $HAM_HOME/bin/ham-toolset repos rclone default

CMD ["echo", "I/ham-base docker image."]
