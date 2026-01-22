```shell
luarocks install lapis
```

```shell
brew --prefix openssl
# >> ..../openssl@3
```

```
# >> luarocks install cqueues CRYPTO_DIR="{above value}" OPENSSL_DIR="{above value}"

ex>>

luarocks install luaossl OPENSSL_DIR=/opt/homebrew/opt/openssl@3 CRYPTO_DIR=/opt/homebrew/opt/openssl@3
luarocks install cqueues CRYPTO_DIR="/opt/homebrew/opt/openssl@3" OPENSSL_DIR="/opt/homebrew/opt/openssl@3"
luarocks install http               

```

```shell
lapis new --cqueues
lapis server
```

