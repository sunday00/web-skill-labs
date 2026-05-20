#!/usr/bin/env bash
set -euo pipefail

REMOTE_USER=""
REMOTE_HOST=""
REMOTE_PORT="22"
REMOTE_PASSWORD=""
REMOTE_BASE="/home/proam/projects"

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

usage() {
  echo "Usage: $0 api|console-api|all"
  echo
  echo "스크립트 상단의 REMOTE_USER, REMOTE_HOST를 먼저 입력하세요."
  echo "비밀번호 자동 입력이 필요하면 REMOTE_PASSWORD에 입력하거나 SSHPASS 환경변수로 전달하세요."
  echo "예: SSHPASS='password' $0 api"
}

select_target() {
  {
    echo "배포 대상을 선택하세요."
    echo "1. api"
    echo "2. console-api"
    echo "3. all"
  } >&2
  read -r -p "> " choice

  case "$choice" in
    1 | api)
      echo "api"
      ;;
    2 | console | console-api)
      echo "console-api"
      ;;
    3 | all)
      echo "all"
      ;;
    *)
      echo "알 수 없는 선택입니다: $choice" >&2
      exit 1
      ;;
  esac
}

require_remote() {
  if [[ -z "$REMOTE_USER" || -z "$REMOTE_HOST" ]]; then
    usage
    exit 1
  fi
}

remote_target() {
  echo "${REMOTE_USER}@${REMOTE_HOST}"
}

ssh_cmd() {
  echo "${SSH_BASE_CMD[*]}"
}

run_ssh() {
  "${SSH_BASE_CMD[@]}" "$(remote_target)" "export NVM_DIR=\"\$HOME/.nvm\"; [ -s \"\$NVM_DIR/nvm.sh\" ] && . \"\$NVM_DIR/nvm.sh\"; $*"
}

run_rsync() {
  rsync -az "$@"
}

prepare_ssh() {
  SSH_BASE_CMD=(ssh -p "$REMOTE_PORT")
  export SSHPASS="${SSHPASS:-$REMOTE_PASSWORD}"

  if [[ -n "$SSHPASS" ]]; then
    if ! command -v sshpass >/dev/null 2>&1; then
      echo "SSHPASS를 사용하려면 sshpass가 필요합니다."
      echo "macOS: brew install hudochenkov/sshpass/sshpass"
      exit 1
    fi

    SSH_BASE_CMD=(sshpass -e ssh -p "$REMOTE_PORT" -o StrictHostKeyChecking=accept-new)
  fi
}

ensure_file() {
  local file="$1"

  if [[ ! -f "$file" ]]; then
    echo "필수 파일이 없습니다: $file"
    exit 1
  fi
}

deploy_app() {
  local name="$1"
  local local_dir="$2"
  local remote_dir="$3"
  local pm2_name="$4"

  echo "==> ${name}: 빌드 시작"
  (cd "$local_dir" && pnpm run build:dev)

  ensure_file "${local_dir}/dist/main.js"
  ensure_file "${local_dir}/.env.dev"
  ensure_file "${local_dir}/ecosystem.config.js"
  ensure_file "${local_dir}/run.sh"

  echo "==> ${name}: 서버 디렉터리 준비"
  run_ssh "mkdir -p '${remote_dir}/dist'"

  echo "==> ${name}: 파일 업로드"
  run_rsync --delete -e "$(ssh_cmd)" "${local_dir}/dist/" "$(remote_target):${remote_dir}/dist/"
  run_rsync -e "$(ssh_cmd)" \
    "${local_dir}/.env.dev" \
    "${local_dir}/ecosystem.config.js" \
    "${local_dir}/run.sh" \
    "$(remote_target):${remote_dir}/"

  echo "==> ${name}: PM2 재시작"
  run_ssh "cd '${remote_dir}' && chmod +x run.sh && ./run.sh"

  echo "==> ${name}: 상태 확인"
  run_ssh "pm2 status '${pm2_name}'"
}

deploy_api() {
  deploy_app \
    "api" \
    "${ROOT_DIR}/pro-am-api-v2" \
    "${REMOTE_BASE}/api" \
    "api"
}

deploy_console_api() {
  deploy_app \
    "console-api" \
    "${ROOT_DIR}/pro-am-console-api-v2" \
    "${REMOTE_BASE}/console-api" \
    "c-api"
}

main() {
  local target="${1:-}"

  if [[ -z "$target" ]]; then
    target="$(select_target)"
  fi

  require_remote
  prepare_ssh

  case "$target" in
    api)
      deploy_api
      ;;
    console-api)
      deploy_console_api
      ;;
    all)
      deploy_api
      deploy_console_api
      ;;
    *)
      usage
      exit 1
      ;;
  esac
}

main "$@"
