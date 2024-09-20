#!/bin/bash -e
. hat repos default shell_linter

log_info "The ham-bash-lib.sh"
(
  set -x
  bash "$HAM_HOME/bin/ham-bash-lib-tests.sh"
)

log_info "Build ham"
(
  set -x
  cd "$HAM_HOME/sources/ham"
  ./build-"${HAM_BIN_LOA}".sh
)

log_info "Run ham linter"
(
  set -x
  ham lint
)

log_info "Run ham tests"
(
  set -x
  cd "$HAM_HOME/sources/ham/tests/ham_base"
  ham all
)

log_info "Build and run pi with ham"
(
  set -x
  ham -X ham/sources/ham/tests/pi Run_pi
)

log_info "Test examples"
(
  set -x
  ham -X ham/sources/examples/py3-venv-web-scrape _run_ci.sh
)

log_info "Build cppm modules."
case $HAM_OS in
  NT* | OSX* | LINUX*)
    (
      set -x
      "$HAM_HOME/sources/ham/tests/ham-test-thrift/_run_ci.sh"
    )
    ;;
  *)
    echo "E/Toolset: Unsupported host OS"
    return 1
    ;;
esac

log_info "Done. ham/_run_ci ran successfully."
