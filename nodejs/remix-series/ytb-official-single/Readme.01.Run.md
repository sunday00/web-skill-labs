# Remix Standard

## depends on

- create new remix
- tailwindcss v3
- daisyui v4
- vite v5

- node v20.18.0
- pnpm 10.4.1

## env 문제

- vite 에서는 .env.local 이 상위 env로 항상 잡히는 문제가....
- 더 좋은 방법이 있을텐데 도저히 그럴 감당이 안되서 임의로 다음과 같이 처리 하였으므로 참고

### 개인 env

- 파일명 `_env.local` 로 작성 후 필요 부분 작성

### 동작

- run.[].sh 쉘 파일로 _env.* 가 .env 덮어 쓰면서 동작
- `pnpm dev:local`

## vite-plugin-mkcert
- apple 로그인 구현 문제로 로컬 ssl 필요할 수 있음.
- `/etc/hosts` 파일 수정하여 ssl 적용필요.

