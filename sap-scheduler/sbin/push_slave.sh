#!/bin/sh

##############################################################
# Program: push_slave.sh
# Author: Tender Xie
# Description:
#   (1) push dags from master to slave (*.py)
#   (2) push plugins from master to slave (*.py)
#   (3) push airflow.cfg from master to slave
# History:
#   (1) create
##############################################################

# config for master
CFG_MASTER_HOME="/home/ap/airflow"
CFG_MASTER_PATH="/home/ap/airflow/airflow_bin"

# config for slave
CFG_SLAVE_HOME="/home/ap/airflow"
CFG_SLAVE_PATH="/home/ap/airflow/airflow_bin"

# check store directory
[ ! -d "${CFG_MASTER_PATH}" ] && echo "[ ! -d ${CFG_MASTER_PATH} ],please check!" && exit 1

# foreach slave
slaves=("airflow@192.168.8.129")

for slave in ${slaves[*]}
do
  echo "[**] slave=${slave}"

  # rsync dags
  echo "1 - rsync dags"
  rsync -rvP "${CFG_MASTER_PATH}/dags/" "${slave}:${CFG_SLAVE_PATH}/dags" --exclude="__init__.py" --include="*.py" --exclude="*" --delete

  # rsync plugins
  echo "2 - rsync plugins"
  rsync -rvP "${CFG_MASTER_PATH}/plugins/operators" "${slave}:${CFG_SLAVE_PATH}/plugins" --exclude="__init__.py" --include="*.py" --delete
  rsync -rvP "${CFG_MASTER_PATH}/plugins/hooks" "${slave}:${CFG_SLAVE_PATH}/plugins" --exclude="__init__.py" --include="*.py" --delete

  # rsync airflow.cfg
  echo "3 - rsync airflow.cfg"
  rsync -rvP "${CFG_MASTER_PATH}/" "${slave}:${CFG_SLAVE_PATH}" --include="airflow.cfg" --exclude="*" --delete

  echo "4 - rsync config"
  rsync -rvP "${CFG_MASTER_PATH}/config/" "${slave}:${CFG_SLAVE_PATH}/config" --include="*.yaml" --exclude="*" --delete
done

exit 0