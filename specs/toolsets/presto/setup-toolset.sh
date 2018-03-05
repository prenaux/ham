#!/bin/bash

# toolset
export HAM_TOOLSET=PRESTO
export HAM_TOOLSET_VER=1
export HAM_TOOLSET_NAME=presto
export HAM_TOOLSET_DIR="${HAM_HOME}/toolsets/presto"

# path setup
case $HAM_OS in
    OSX*)
        export PRESTO_HOME="${HAM_TOOLSET_DIR}/jvm/"
        export PATH="${PRESTO_HOME}/bin":${PATH}
        if [ ! -e "$PRESTO_HOME" ]; then
            toolset_dl presto presto_jvm
            if [ ! -e "$PRESTO_HOME" ]; then
                echo "E/jvm folder doesn't exist in the toolset"
                return 1
            fi
        fi
        ;;
    *)
        echo "E/Toolset: Unsupported host OS"
        return 1
        ;;
esac

export PATH="${HAM_TOOLSET_DIR}":${PATH}

export PRESTO_DB_DIR="$WORK/Server/presto"

# Setup the configuration
mkdir -p "$PRESTO_HOME/etc"
mkdir -p "$PRESTO_HOME/etc/catalog"

cat <<EOF>> "$PRESTO_HOME/etc/jvm.config"
-server
-Xmx16G
-XX:+UseG1GC
-XX:G1HeapRegionSize=32M
-XX:+UseGCOverheadLimit
-XX:+ExplicitGCInvokesConcurrent
-XX:+HeapDumpOnOutOfMemoryError
-XX:+ExitOnOutOfMemoryError
EOF

cat <<EOF>> "$PRESTO_HOME/etc/node.properties"
node.environment=production
node.id=A512B8A0-B0E9-A940-89BD-4DB9E91272F7
node.data-dir=${PRESTO_DB_DIR}/data
EOF

cat <<EOF>> "$PRESTO_HOME/etc/config.properties"
coordinator=true
node-scheduler.include-coordinator=true
http-server.http.port=8066
query.max-memory=5GB
query.max-memory-per-node=1GB
discovery-server.enabled=true
discovery.uri=http://example.net:8066
EOF

cat <<EOF>> "$PRESTO_HOME/etc/log.properties"
com.facebook.presto=INFO
EOF

cat <<EOF>> "$PRESTO_HOME/etc/catalog/jmx.properties"
connector.name=jmx
EOF

# Version number
VER="--- presto ---------------------
Presto `basename $(ls $PRESTO_HOME/lib/presto-client*) | sed s~presto-client-~~ | sed s~.jar~~`"
if [ $? != 0 ]; then
    echo "E/Can't get version."
    return 1
fi
export HAM_TOOLSET_VERSIONS="$HAM_TOOLSET_VERSIONS
$VER"
