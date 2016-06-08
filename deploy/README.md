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
