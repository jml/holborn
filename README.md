# Code search

[![Build Status](https://travis-ci.org/jml/holborn.svg?branch=master)](https://travis-ci.org/jml/holborn)

Current project goal is to be able to browse a Git repository of code, being
able to click on elements of the code and jump to their definitions, or to
where they are used.

Our intent is that this repository will contain multiple Haskell packages. The
main one is `holborn-web`.


# Notes for production setup.

SSL termination can be done with stunnel:

http://serverfault.com/questions/440225/multiple-ssl-certs-with-stunnel


# Running sshd

NB you need to make your own `testconf` with absolute paths to the keys

Only once:

```
ssh-keygen -f ./conf/ssh_host_rsa_key -N '' -t rsa
ssh-keygen -f ./conf/ssh_host_dsa_key -N '' -t dsa
```

After that:

```
nix-shell -p "callPackage ./nix/holborn-ssh.nix {}"
sshd -D -e -f conf/testconf-tom
```
