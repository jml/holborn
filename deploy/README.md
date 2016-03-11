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
