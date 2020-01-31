# fingerd

#### insert use sqlite3
```bash
sqlite3
> .open finger.db
> SELECT * FROM users;
> INSERT INTO users (username,shell,homeDirectory,realName,phone) VALUES ('qodjf','/bin/zsh','/Users/qodjf','qodjf','12345678' );
```
