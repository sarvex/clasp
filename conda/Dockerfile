FROM continuumio/miniconda3:latest

SHELL ["/bin/bash", "-c"]

ENV HOME /home/root

RUN apt-get update && \
    apt-get install -o Dpkg::Options::="--force-overwrite" -y \
        nano wget sudo ecl sbcl git locales curl

RUN echo 'en_US.UTF-8 UTF-8' >/etc/locale.gen
RUN sudo -E locale-gen

WORKDIR ${HOME}

COPY . ${HOME}/conda

RUN wget https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --non-interactive --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --eval "(ql-util:without-prompting (ql:add-to-init-file))" && \
    rm quicklisp.lisp && \
    conda init && \
    conda install -y conda-build
