# Deploying

```
nixops deploy
nixops send-keys
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

Dex supports automatic endpoint discovery but we're just hard-coding
the endpoints for now because everything is under our control. If you
want to check the endpoints see:

```
$ curl 127.0.0.1:5556/.well-known/openid-configuration | json_pp
{
   "token_endpoint" : "http://127.0.0.1:5556/token",
}
```
