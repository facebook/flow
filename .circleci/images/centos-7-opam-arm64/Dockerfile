# This file is copied from centos-7-opam-amd64, but replaces `FROM` with arm64 version

FROM arm64v8/centos:centos7

# FROM centos:7
LABEL distro_style="rpm"
RUN yum --version || dnf install -y yum
RUN touch /var/lib/rpm/*
RUN yum install -y yum-plugin-ovl && yum clean all
RUN yum update -y
RUN yum groupinstall -y "Development Tools" && yum clean all
RUN yum install -y sudo passwd bzip2 patch rsync nano gcc-c++ git tar curl xz libX11-devel bubblewrap which m4 diffutils findutils which tar curl xz libcap-devel openssl && yum clean all
RUN git config --global user.email "docker@example.com"
RUN git config --global user.name "Docker"
RUN curl -fOL https://github.com/projectatomic/bubblewrap/releases/download/v0.4.1/bubblewrap-0.4.1.tar.xz
RUN tar xf bubblewrap-0.4.1.tar.xz
RUN cd bubblewrap-0.4.1 && ./configure --prefix=/usr/local && make && sudo make install
RUN rm -rf bubblewrap-0.4.1.tar.xz bubblewrap-0.4.1
RUN git clone -b 2.0 git://github.com/ocaml/opam /tmp/opam
RUN sh -c "cd /tmp/opam && make cold && mkdir -p /usr/bin && cp /tmp/opam/opam /usr/bin/opam-2.0 && chmod a+x /usr/bin/opam-2.0 && rm -rf /tmp/opam"
RUN git clone -b master git://github.com/ocaml/opam /tmp/opam
RUN sh -c "cd /tmp/opam && make cold && mkdir -p /usr/bin && cp /tmp/opam/opam /usr/bin/opam-master && chmod a+x /usr/bin/opam-master && rm -rf /tmp/opam"

FROM arm64v8/centos:centos7
# FROM centos:7
RUN yum --version || dnf install -y yum
RUN touch /var/lib/rpm/*
RUN yum install -y yum-plugin-ovl && yum clean all
RUN yum update -y
RUN yum groupinstall -y "Development Tools" && yum clean all
RUN yum install -y sudo passwd bzip2 patch rsync nano gcc-c++ git tar curl xz libX11-devel bubblewrap which m4 diffutils findutils && yum clean all
COPY --from=0 [ "/usr/local/bin/bwrap", "/usr/bin/bwrap" ]
COPY --from=0 [ "/usr/bin/opam-2.0", "/usr/bin/opam-2.0" ]
COPY --from=0 [ "/usr/bin/opam-master", "/usr/bin/opam-2.1" ]
RUN ln /usr/bin/opam-2.0 /usr/bin/opam
RUN sed -i.bak '/LC_TIME LC_ALL LANGUAGE/aDefaults    env_keep += "OPAMYES OPAMJOBS OPAMVERBOSE"' /etc/sudoers
RUN echo 'opam ALL=(ALL:ALL) NOPASSWD:ALL' > /etc/sudoers.d/opam
RUN chmod 440 /etc/sudoers.d/opam
RUN chown root:root /etc/sudoers.d/opam
RUN sed -i.bak 's/^Defaults.*requiretty//g' /etc/sudoers
RUN useradd -d /home/opam -u 1000 -m -s /bin/bash opam
RUN passwd -l opam
RUN chown -R opam:opam /home/opam
USER opam
ENV HOME="/home/opam"
WORKDIR /home/opam
RUN mkdir .ssh
RUN chmod 700 .ssh
RUN echo 'wrap-build-commands: []' > ~/.opamrc-nosandbox
RUN echo 'wrap-install-commands: []' >> ~/.opamrc-nosandbox
RUN echo 'wrap-remove-commands: []' >> ~/.opamrc-nosandbox
RUN echo 'required-tools: []' >> ~/.opamrc-nosandbox
RUN echo '#!/bin/sh' > /home/opam/opam-sandbox-disable
RUN echo 'cp ~/.opamrc-nosandbox ~/.opamrc' >> /home/opam/opam-sandbox-disable
RUN echo 'echo --- opam sandboxing disabled' >> /home/opam/opam-sandbox-disable
RUN chmod a+x /home/opam/opam-sandbox-disable
RUN sudo mv /home/opam/opam-sandbox-disable /usr/bin/opam-sandbox-disable
RUN echo 'wrap-build-commands: ["%{hooks}%/sandbox.sh" "build"]' > ~/.opamrc-sandbox
RUN echo 'wrap-install-commands: ["%{hooks}%/sandbox.sh" "install"]' >> ~/.opamrc-sandbox
RUN echo 'wrap-remove-commands: ["%{hooks}%/sandbox.sh" "remove"]' >> ~/.opamrc-sandbox
RUN echo '#!/bin/sh' > /home/opam/opam-sandbox-enable
RUN echo 'cp ~/.opamrc-sandbox ~/.opamrc' >> /home/opam/opam-sandbox-enable
RUN echo 'echo --- opam sandboxing enabled' >> /home/opam/opam-sandbox-enable
RUN chmod a+x /home/opam/opam-sandbox-enable
RUN sudo mv /home/opam/opam-sandbox-enable /usr/bin/opam-sandbox-enable
RUN git config --global user.email "docker@example.com"
RUN git config --global user.name "Docker"
COPY --chown=opam:opam [ ".", "/home/opam/opam-repository" ]
RUN opam-sandbox-disable
RUN opam init -k local -a /home/opam/opam-repository --bare
RUN rm -rf .opam/repo/default/.git
COPY [ "Dockerfile", "/Dockerfile.opam" ]