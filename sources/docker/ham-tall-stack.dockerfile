# Start from the nodejs image for shorter build time
FROM ham-nodejs

# Update to the latest ham
WORKDIR $HAM_HOME
RUN bash -c "source $HAM_HOME/bin/ham-toolset repos > /dev/null && git-update ."
RUN git log -3 --oneline

# The default stuff that should already be there
RUN $HAM_HOME/bin/ham-toolset repos rclone default
RUN $HAM_HOME/bin/ham-toolset nodejs

# Our toolsets
RUN $HAM_HOME/bin/ham-toolset postgres
RUN $HAM_HOME/bin/ham-toolset secrets_doppler
RUN $HAM_HOME/bin/ham-toolset nginx
RUN $HAM_HOME/bin/ham-toolset php_8

CMD ["echo", "I/ham-tall-stack docker image."]
