# This docker container is meant for debugging mu4e-views with different mu versions
# A typical use case is to start a container with your local development version of mu4e-views
# E.g., from your local mu4e-views git repo: docker run -ti --rm -v $(pwd):/mu4e-views mu4e-views-test /bin/bash
# There are scripts to start emacs with a particular mu version, e.g., ./emu-master.sh or ./emu-1.4.13.sh
# To build the docker image copy some valid Maildir to ./Maildir and run "docker build -t mu4e-views-test ."
# If you need to test with a different version, then add it to VERSIONS="1.3.10 1.4.13 master" in dockerfiles/build-mu.sh
FROM silex/emacs:master-ci-eldev
########################################
# get mu src
########################################
RUN git clone https://github.com/djcb/mu /mu-src
########################################
# Copy Maildir
########################################
COPY ./Maildir /Maildir
########################################
# Install packages and emacs
########################################
RUN apt-get update && apt-get -y --no-install-recommends install \
    automake                         \
    autoconf-archive                 \
    autotools-dev                    \
    libglib2.0-dev                   \
    libxapian-dev			         \
    libgmime-3.0-dev                 \
    m4                               \
    make                             \
    libtool                          \
    pkg-config                       \
    g++                              \
    isync                            \
    meson                            \
    ninja-build                      \
    && rm -rf /var/lib/apt/lists/*
########################################
# Copy build script and build mu versions
########################################
RUN rm -rf /mu-src && git clone https://github.com/djcb/mu /mu-src
COPY ./dockerfiles/build-mu.sh /
RUN chmod 744 /build-mu.sh && /build-mu.sh
########################################
# Copy mu4e-views repo
########################################
COPY . /mu4e-views
RUN find /mu4e-views -name \*.elc -execdir rm {} \;
########################################
# Make sure emacs packages we need are installed
########################################
#RUN MUVER=1.6.4 emacs --batch -l /mu4e-views/testconfig/test-file.el
WORKDIR /
