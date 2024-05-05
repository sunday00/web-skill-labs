## install
```shell
docker compose up -d
```

## add gui
- https://www.rabbitmq.com/docs/management

```shell
sudo docker exec -it rabbitmq-local /bin/sh

# 
rabbitmq-plugins enable rabbitmq_management
```

- open browser and go to  http://localhost:15672



