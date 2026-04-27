docker exec -it mongo1 mongosh --eval "rs.initiate({
_id: 'proAmReplicaSet',
members: [
{ _id: 0, host: 'mongo1:27017' },
{ _id: 1, host: 'mongo2:27017' },
{ _id: 2, host: 'mongo3:27017' }
]
})"


docker exec -it mongo1 mongosh --eval "rs.status()"


