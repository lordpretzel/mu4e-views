#!/bin/bash
VERSIONS="v1.8.14 v1.10.1 master" # v1.10.1" # v1.8.14 v1.10.1 master"
cd /mu-src

check_exit()
{
    if [ $? != 0 ]; then
        echo "building ${ver} failed"
        exit 1
    fi
}

for ver in ${VERSIONS};
do
    echo "build mu ${ver}"
    git clean
    git checkout ${ver}
    ./autogen.sh
    # ./configure --prefix /mu-${ver}
    meson setup --wipe --prefix /mu-${ver} build \
        && ninja -C build \
        && meson install -C build
    make
    make install
    check_exit
    /mu-${ver}/bin/mu init -m /Maildir --muhome=/mu-home-${ver} --my-address "lord_pretzel@gmx.net"
    /mu-${ver}/bin/mu --muhome=/mu-home-${ver} index
    echo "MUVER=${ver} emacs -l /mu4e-views/testconfig/test-file.el" > /emu-${ver}.sh
    chmod 744 /emu-${ver}.sh
done

# 0.9.15
# 0.9.16
# 0.9.17
# 0.9.18
# 0.9.9.6pre3
# 1,4.7
# 1.0-alpha0
# 1.0-alpha1
# 1.0-alpha2
# 1.0-alpha3
# 1.2
# 1.2-rc1
# 1.3.1
# 1.3.10
# 1.3.3
# 1.3.5
# 1.3.6
# 1.3.9
# 1.4
# 1.4.1
# 1.4.10
# 1.4.11
# 1.4.12
# 1.4.13
# 1.4.14          Stable release 1.4.14
# 1.4.15
# 1.4.2
# 1.4.3
# 1.4.4
# 1.4.5
# 1.4.6
# 1.4.7
# 1.4.8
# 1.4.9
# 1.6.0
# list
# v0.6
# v0.6-beta
# v0.7
# v0.7-beta
# v0.8
# v0.8-beta
# v0.9
# v0.9-beta
# v0.9.1
# v0.9.10
# v0.9.11
# v0.9.12
# v0.9.13
# v0.9.16
# v0.9.2
# v0.9.3
# v0.9.4
# v0.9.5
# v0.9.6
# v0.9.7
# v0.9.7-pre
