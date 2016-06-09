# Deploying

```
nixops deploy
```

# Preparing the DB

```
nixops ssh web
[root@web:~]# createuser holborn
[root@web:~]# createdb holborn -O holborn
```


# Dex

Prepare the DB

```
nixops ssh web
[root@web:~]# createuser dex-rw
[root@web:~]# createdb dex -O dex-rw
```

We're registering a local "connector" automatically.


## Dex endpoints:

```
$ curl 127.0.0.1:5556/.well-known/openid-configuration | json_pp
{
   "token_endpoint" : "http://127.0.0.1:5556/token",
   "id_token_signing_alg_values_supported" : [
      "RS256"
   ],
   "grant_types_supported" : [
      "authorization_code",
      "client_credentials"
   ],
   "subject_types_supported" : [
      "public"
   ],
   "issuer" : "http://127.0.0.1:5556",
   "authorization_endpoint" : "http://127.0.0.1:5556/auth",
   "jwks_uri" : "http://127.0.0.1:5556/keys",
   "response_types_supported" : [
      "code"
   ],
   "token_endpoint_auth_methods_supported" : [
      "client_secret_basic"
   ]
}
```
