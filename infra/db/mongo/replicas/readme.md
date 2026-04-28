# run docker to prepare ubuntu
```shell
docker compose up -d
docker exec -it mongo_repl /bin/bash
```

# ubuntu update
```shell
apt update 
apt upgrade
```

# install some dep
```shell
apt install curl gpg gnupg
```

# register repository
```shell
curl -fsSL https://www.mongodb.org/static/pgp/server-7.0.asc | \
  gpg -o /usr/share/keyrings/mongodb-server-7.0.gpg \
   --dearmor --yes

echo "deb [ arch=amd64,arm64 signed-by=/usr/share/keyrings/mongodb-server-7.0.gpg ] https://repo.mongodb.org/apt/ubuntu jammy/mongodb-org/7.0 multiverse" | tee /etc/apt/sources.list.d/mongodb-org-7.0.list

apt update
```

# install mongod
```shell
install -y mongodb-org

# then choose 5 asia, 68 seoul
```

# run mongodb 
```shell
mkdir /data/db/data1
mkdir /data/db/data2
mkdir /data/db/data3
mkdir /data/db/logs

mongod --dbpath /data/db/data1 --port 27217 --replSet proam2 --fork --logpath /data/db/logs/log --bind_ip_all
mongod --dbpath /data/db/data2 --port 27218 --replSet proam2 --fork --logpath /data/db/logs/log --bind_ip_all
mongod --dbpath /data/db/data3 --port 27219 --replSet proam2 --fork --logpath /data/db/logs/log --bind_ip_all

```

# replica setting
```shell
mongosh --port 27217

rs.initiate({
  _id: 'proam2',
  members: [
    { _id: 0, host: '127.0.0.1:27217', priority: 1 },
    { _id: 1, host: '127.0.0.1:27218', priority: 0.5 },
    { _id: 2, host: '127.0.0.1:27219', priority: 0.5 }
  ]
})

rs.status()

rs.isMaster().ismaster
# true is fine
```