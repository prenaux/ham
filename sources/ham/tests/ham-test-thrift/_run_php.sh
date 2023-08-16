#!/bin/bash -ex
cd src_php
if [ ! -e "vendor/autoload.php" ]; then
  composer install
fi
php PhpClient.php
