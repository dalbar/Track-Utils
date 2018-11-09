FROM alpine

RUN cat /etc/resolv.conf
RUN ping 8.8.8.8
RUN apk update
RUN apk add curl
RUN sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
COPY . .
RUN opam install .
RUN dune test
