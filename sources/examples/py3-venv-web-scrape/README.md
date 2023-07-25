# Overview

A simple web scraper to test py3-venv.

## Notes

- `py3_pip_install.txt` is the list of python libraries to install. `py3-venv-pip-install` is the best way to add modules, but you can also edit the `py3_pip_install.txt` file and run `py3-venv-pip-reinstall`.
- `py3_requirements.txt` are the current versions installed, basically the lock file of other package managers.

## Development

Get started:
```
cd $WORK/pierre/py-web-scrape
. hat
# Calls 'source py3-venv-activate' to setup the python3 venv and download all
# the dependencies if necessary.
```

Install a new package:
```
py3-venv-pip-install foobar
# Adds foobar to py3_pip_install.txt & does 'py3-venv-requirements-freeze' to
# update py3_requirements.txt with the exact versions used.
```

Upgrade all packages:
```
py3-venv-requirements-upgrade
```
