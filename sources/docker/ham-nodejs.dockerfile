FROM ham-base

# Update to the latest ham
WORKDIR $HAM_HOME
RUN bash -c "source $HAM_HOME/bin/ham-toolset repos > /dev/null && git-update ."
RUN git log -3 --oneline

# Install nodejs
RUN $HAM_HOME/bin/ham-toolset nodejs

CMD ["echo", "I/ham-nodejs docker image."]
