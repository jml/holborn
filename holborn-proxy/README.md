# holborn-proxy

Internet facing frontend webservice. All HTTP traffic to Holborn goes
through this. Think of it as an andvanced, not-c nginx.


# TODO

* state parameter - do we need it?
  http://www.twobotechnologies.com/blog/2014/02/importance-of-state-in-oauth2.html


# Generated keys for development

Follow instructions here:

https://devcenter.heroku.com/articles/ssl-certificate-self


# How to run

```
cabal run -- --public-host=https://127.0.0.1:8443 --upstream-host=127.0.0.1 --upstream-port=8002 --ssl-full-chain=server.crt --ssl-key=server.key --dex-host=http://norf.co:5556  --ssl-port=8443 --oauth-client-id="$HOLBORN_OAUTH_CLIENT_ID" --oauth-client-secret="$HOLBORN_OAUTH_CLIENT_SECRET"
```
