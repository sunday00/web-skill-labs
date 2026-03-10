# config server init

```shell
# docker exec then in container
mongosh --port 27119 --eval 'rs.initiate({_id: "configRS", configsvr: true, members: [{_id: 0, host: "configsvr:27119"}]})'
```

# shards init 

```shell
# docker exec then in container
mongosh --port 27118 --eval 'rs.initiate({_id: "shard1RS", members: [{_id: 0, host: "shard1:27118"}]})'
mongosh --port 27120 --eval 'rs.initiate({_id: "shard2RS", members: [{_id: 0, host: "shard2:27120"}]})'
```

# register shards

```shell
# docker exec then in container
mongosh --port 27117 --eval 'sh.addShard("shard1RS/shard1:27118"); sh.addShard("shard2RS/shard2:27120");'
```