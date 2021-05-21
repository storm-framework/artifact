FROM ubuntu:20.04

# git to get the artifact
# wget and unzip for the rest of the setup
# libtinfo-dev and locales for Haskell's I/O to work
# python3 for the 'fig9.py' script
RUN apt-get update && apt-get install -y git wget unzip libtinfo-dev locales python3

# Configure the right locale for Haskell
RUN locale-gen en_US.UTF-8
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL=en_US.UTF-8

# Install Stack
RUN wget -qO- https://get.haskellstack.org/ | sh
ENV PATH=/root/.local/bin:$PATH

# Install Z3
RUN wget https://github.com/Z3Prover/z3/releases/download/z3-4.8.10/z3-4.8.10-x64-ubuntu-18.04.zip
RUN unzip z3-4.8.10-x64-ubuntu-18.04.zip
RUN rm z3-4.8.10-x64-ubuntu-18.04.zip
ENV PATH=/z3-4.8.10-x64-ubuntu-18.04/bin/:$PATH

# Install Tokei
RUN wget https://github.com/XAMPPRocky/tokei/releases/download/v12.1.2/tokei-x86_64-unknown-linux-gnu.tar.gz
RUN tar -xf tokei-x86_64-unknown-linux-gnu.tar.gz
RUN rm tokei-x86_64-unknown-linux-gnu.tar.gz
ENV PATH=/:$PATH

# Clone the artifact
RUN git clone --recurse-submodules https://github.com/storm-framework/artifact.git

# Clean up
RUN apt-get purge -y --autoremove wget unzip
RUN rm -rf /var/lib/apt/lists/*
