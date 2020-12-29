# This docker container is meant for debugging mu4e-views with different mu versions
# A typical use case is to start a container with your local development version of mu4e-views
# E.g., from your local mu4e-views git repo: docker run -ti --rm -v $(pwd):/mu4e-views mu4e-views-test /bin/bash
# There are scripts to start emacs with a particular mu version, e.g., ./emu-master.sh or ./emu-1.4.13.sh
# To build the docker image copy some valid Maildir to ./Maildir and run "docker build -t mu4e-views-test ."
# If you need to test with a different version, then add it to VERSIONS="1.3.10 1.4.13 master" in dockerfiles/build-mu.sh
FROM silex/emacs:master-ci-eldev
RUN git clone https://github.com/djcb/mu /mu-src
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
	&& rm -rf /var/lib/apt/lists/*
RUN rm -f /mu4e-views/testconfig/test-file.elc
WORKDIR /mu-src
COPY ./Maildir /Maildir
COPY ./dockerfiles/build-mu.sh /
RUN chmod 744 /build-mu.sh && /build-mu.sh
RUN mkdir -p /Maildir/lordpretzel-gmx/Drafts /Maildir/lordpretzel-gmx/Trash
COPY . /mu4e-views
RUN find /mu4e-views -name \*.elc -execdir rm {} \;
RUN MUVER=master emacs --batch -l /mu4e-views/testconfig/test-file.el
WORKDIR /
