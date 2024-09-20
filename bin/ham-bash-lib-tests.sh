#!/bin/bash
#
# run:
#   bash "$HAM_HOME/bin/ham-bash-lib-tests.sh"
#
if [[ -z "$HAM_HOME" ]]; then
  echo "E/HAM_HOME not set !"
  exit 1
fi
. "$HAM_HOME/bin/ham-bash-lib.sh"
. "$HAM_HOME/bin/ham-bash-lib-unittest.sh"

log_info "HAM_OS_PACKAGE_MANAGER: ${HAM_OS_PACKAGE_MANAGER}"

###########################################################################
# TESTS: ham_os_package
###########################################################################
FIXTURE "ham_os_package"

DETECTED_PACKAGE_MANAGER=$(ham_os_package_detect_default_manager)
log_info "DETECTED_PACKAGE_MANAGER: ${DETECTED_PACKAGE_MANAGER}"

# shellcheck disable=SC2317
test__ham_os_package__default_package_manager() {
  local FUNC=test__ham_os_package__default_package_manager
  case "$HAM_OS" in
    NT*)
      CHECK_EQUAL "none" "$DETECTED_PACKAGE_MANAGER" "Windows package manager should be none." || return 1
      ;;
    LINUX*)
      if has_inpath pacman; then
        CHECK_EQUAL "pacman" "$DETECTED_PACKAGE_MANAGER" "Arch/SteamOS package manager should be pacman." || return 1
      else
        CHECK_EQUAL "apt" "$DETECTED_PACKAGE_MANAGER" "Ubuntu/Debian package manager should be apt." || return 1
      fi
      ;;
    OSX*)
      CHECK_EQUAL "brew" "$DETECTED_PACKAGE_MANAGER" "macOS package manager should be brew." || return 1
      ;;
    *)
      complain "$FUNC" "Unknown HAM_OS: $HAM_OS"
      return 1
      ;;
  esac

  CHECK_NOT_EQUAL "" "$HAM_OS_PACKAGE_MANAGER" "HAM_OS_PACKAGE_MANAGER shouldn't be empty" || return 1
}
TEST "ham_os_package" "test__ham_os_package__default_package_manager"

if [[ "$HAM_OS_PACKAGE_MANAGER" != "none" && "$HAM_OS_PACKAGE_MANAGER" != "brew" ]]; then
  # shellcheck disable=SC2317
  test__ham_os_package__find_in_usr_dir() {
    local value
    value=$(ham_os_package_find_in_usr_dir include "curl/curl.h")
    log_info "value: $value"
    CHECK_CONTAINS "curl/curl.h" "$value" "curl/curl.h should exist" || return 1
  }
  TEST "ham_os_package" "test__ham_os_package__find_in_usr_dir"
fi

###########################################################################
# TESTS: ham_os_package_get_value_tests
###########################################################################
FIXTURE "ham_os_package_get_value_tests"

# shellcheck disable=SC2317
test_matching_key_apt() {
  CHECK_EQUAL "binutils-dev" "$(ham_os_package_get_value "apt" "apt:binutils-dev" "pacman:binutils" "brew:binutils" "default_value")" || return 1
}
TEST "ham_os_package_get_value_tests" "test_matching_key_apt"

# shellcheck disable=SC2317
test_matching_key_pacman() {
  CHECK_EQUAL "binutils" "$(ham_os_package_get_value "pacman" "apt:binutils-dev" "pacman:binutils" "brew:binutils" "default_value")" || return 1
}
TEST "ham_os_package_get_value_tests" "test_matching_key_pacman"

# shellcheck disable=SC2317
test_fallback_to_default_value() {
  CHECK_EQUAL "default_value" "$(ham_os_package_get_value "unknown" "apt:binutils-dev" "pacman:binutils" "brew:binutils" "default_value")" || return 1
}
TEST "ham_os_package_get_value_tests" "test_fallback_to_default_value"

# shellcheck disable=SC2317
test_error_multiple_default_values() {
  # NOTE: we cannot use local here because it'll eat the return code
  value=$(ham_os_package_get_value "apt" "apt:binutils-dev" "pacman:binutils" "brew:binutils" "default_value" "another_default" 2>&1)
  CHECK_EQUAL 1 $? "Expecting an error, exit code should be 1." || return 1
  CHECK_CONTAINS "ham_os_package_get_value: Multiple default values specified." "$value" || return 1
}
TEST "ham_os_package_get_value_tests" "test_error_multiple_default_values"

# shellcheck disable=SC2317
test_error_no_match_no_default() {
  ham_os_package_get_value "unknown" "apt:binutils-dev" "pacman:binutils" "brew:binutils"
  CHECK_EQUAL 1 $? "Expecting an error, exit code should be 1." || return 1
}
TEST "ham_os_package_get_value_tests" "test_error_no_match_no_default"

#######################################################################
# TESTS: pacman package manager tests
#######################################################################

test_pacman_package_manager() {
  FIXTURE "pacman_package_tests"

  # shellcheck disable=SC2317
  test_pacman_curl_exists() {
    ham_os_package_exist_pacman "curl"
    CHECK_EQUAL 0 $? "pacman: curl should exist" || return 1
  }
  TEST "pacman_package_tests" "test_pacman_curl_exists"

  # shellcheck disable=SC2317
  test_pacman_non_existent_package() {
    ham_os_package_exist_pacman "wee_iamnotapackage"
    CHECK_EQUAL 1 $? "$()" "pacman: wee_iamnotapackage should not exist" || return 1
  }
  TEST "pacman_package_tests" "test_pacman_non_existent_package"

  # shellcheck disable=SC2317
  test_pacman_curl_already_installed() {
    value=$(ham_os_package_check_and_install_pacman "curl")
    CHECK_EQUAL 0 $? "Already packaged installed should return a success code." || return 1
    CHECK_CONTAINS "Package 'curl' is already installed." "$value" || return 1
  }
  TEST "pacman_package_tests" "test_pacman_curl_already_installed"
}

#######################################################################
# TESTS: apt package manager tests
#######################################################################
test_apt_package_manager() {
  FIXTURE "apt_package_tests"

  # shellcheck disable=SC2317
  test_apt_curl_exists() {
    ham_os_package_exist_apt "curl"
    CHECK_EQUAL 0 $? "apt: curl should exist" || return 1
  }
  TEST "apt_package_tests" "test_apt_curl_exists"

  # shellcheck disable=SC2317
  test_apt_non_existent_package() {
    ham_os_package_exist_apt "wee_iamnotapackage"
    CHECK_EQUAL 1 $? "$()" "apt: wee_iamnotapackage should not exist" || return 1
  }
  TEST "apt_package_tests" "test_apt_non_existent_package"

  # shellcheck disable=SC2317
  test_apt_curl_already_installed() {
    value=$(ham_os_package_check_and_install_apt "curl")
    CHECK_EQUAL 0 $? "Already packaged installed should return a success code." || return 1
    CHECK_CONTAINS "Package 'curl' is already installed." "$value" || return 1
  }
  TEST "apt_package_tests" "test_apt_curl_already_installed"
}

#######################################################################
# TESTS: brew package manager tests
#######################################################################
test_brew_package_manager() {
  FIXTURE "brew_package_tests"

  # shellcheck disable=SC2317
  test_brew_curl_exists() {
    ham_os_package_exist_brew "curl"
    CHECK_EQUAL 0 $? "brew: curl should exist" || return 1
  }
  TEST "brew_package_tests" "test_brew_curl_exists"

  # shellcheck disable=SC2317
  test_brew_non_existent_package() {
    ham_os_package_exist_brew "wee_iamnotapackage"
    CHECK_EQUAL 1 $? "$()" "brew: wee_iamnotapackage should not exist" || return 1
  }
  TEST "brew_package_tests" "test_brew_non_existent_package"

  # shellcheck disable=SC2317
  test_brew_curl_already_installed() {
    value=$(ham_os_package_check_and_install_brew "curl")
    CHECK_EQUAL 0 $? "Already packaged installed should return a success code." || return 1
    CHECK_CONTAINS "Package 'curl' is already installed." "$value" || return 1
  }
  TEST "brew_package_tests" "test_brew_curl_already_installed"
}

#######################################################################
# Run package manager tests based on detection
#######################################################################
detect_package_manager() {
  command -v "$1" &>/dev/null
  return $?
}

if detect_package_manager "pacman"; then
  log_info "pacman detected, adding tests for it"
  test_pacman_package_manager
else
  log_warning "pacman not found, not adding its tests"
fi

if detect_package_manager "apt-get"; then
  log_info "apt-get detected, adding tests for it"
  test_apt_package_manager
else
  log_warning "apt-get not found, not adding its tests"
fi

if detect_package_manager "brew"; then
  log_info "brew detected, adding tests for it"
  test_brew_package_manager
else
  log_warning "brew not found, not adding its tests"
fi

###########################################################################
# TESTS: ham_os_package_syslib
###########################################################################
FIXTURE "ham_os_package_syslib"

if [[ "$HAM_OS_PACKAGE_MANAGER" == "none" ]]; then
  # shellcheck disable=SC2317
  test__ham_os_package_syslib_no_package_manager() {
    CHECK_EQUAL "NT" "$HAM_OS" "Only Windows doesnt have a package manager atm, got '$HAM_OS'." || return 1
  }
  TEST "ham_os_package_syslib" "test__ham_os_package_syslib_no_package_manager"

else
  # shellcheck disable=SC2317
  test__ham_os_package_syslib_find_file__curl_include() {
    local FUNC=test__ham_os_package_syslib_find_file__curl_include
    value=$(ham_os_package_syslib_find_file "include" "curl/curl.h" apt:libcurl4-openssl-dev pacman:curl brew:curl)
    CHECK_EQUAL 0 $? "call should succeed" || return 1
    log_info "... value: $value"
    CHECK_CONTAINS "curl/curl.h" "$value" "curl/curl.h should exist in the include path" || return 1
  }
  TEST "ham_os_package_syslib" "test__ham_os_package_syslib_find_file__curl_include"

  # shellcheck disable=SC2317
  test__ham_os_package_syslib_check_and_install__curl_include() {
    local FUNC=test__ham_os_package_syslib_check_and_install__curl_include
    value=$(ham_os_package_syslib_check_and_install "include" "curl/curl.h" apt:libcurl4-openssl-dev pacman:curl brew:curl)
    CHECK_EQUAL 0 $? "call should succeed" || return 1
    log_info "... value: $value"
    CHECK_CONTAINS "curl/curl.h" "$value" "curl/curl.h should be found after check/install" || return 1
  }
  TEST "ham_os_package_syslib" "test__ham_os_package_syslib_check_and_install__curl_include"

  # shellcheck disable=SC2317
  test__ham_os_package_syslib_check_and_install__autossh_bin() {
    log_info "Uninstalling autossh to test reinstall..."
    case "$HAM_OS_PACKAGE_MANAGER" in
      pacman)
        (
          set -ex
          sudo pacman -R --noconfirm autossh
        )
        ;;
      brew)
        (
          set -ex
          ham-brew uninstall autossh
        )
        ;;
      *)
        log_warning "Don't know how to uninstall autossh with '$HAM_OS_PACKAGE_MANAGER'."
        ;;
    esac
    CHECK_EQUAL 0 $? "uninstall of autossh should succeed" || return 1

    local FUNC=test__ham_os_package_syslib_check_and_install__autossh_bin
    value=$(ham_os_package_syslib_check_and_install "bin" "autossh" autossh)
    CHECK_EQUAL 0 $? "install of autossh should succeed" || return 1
    log_info "... value: $value"
    expected_path=$(ham_os_package_syslib_find_file "bin" "autossh" autossh)
    log_info "... expected_path: $expected_path"
    CHECK_CONTAINS "autossh" "$value" "check_and_install output should at least contain autossh" || return 1
    CHECK_EQUAL "$expected_path" "$value" "check_and_install should be consistent with ham_os_package_syslib_find_file" || return 1
  }
  TEST "ham_os_package_syslib" "test__ham_os_package_syslib_check_and_install__autossh_bin"

  # shellcheck disable=SC2317
  test__ham_os_package_syslib_check_and_install__non_existent_package() {
    local FUNC=test__ham_os_package_syslib_check_and_install__non_existent_package
    value=$(ham_os_package_syslib_check_and_install "include" "wee_iamnotapackage.h" apt:wee_iamnotapackage-bla wee_iamnotapackage 2>&1)
    CHECK_EQUAL 1 $? "call should fail" || return 1
    log_info "... value: $value"
    CHECK_CONTAINS "ham_os_package_syslib_check_and_install: Unable to find 'wee_iamnotapackage.h' after installing package 'wee_iamnotapackage'" "$value" "curl/curl.h should be found after check/install" || return 1
  }
  TEST "ham_os_package_syslib" "test__ham_os_package_syslib_check_and_install__non_existent_package"
fi

###########################################################################
# RUN_TESTS
###########################################################################
RUN_TESTS "$1"
exit $?
