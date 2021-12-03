#!/bin/sh

##############################################################
# Program: sync_slave.sh
# Author: Tender Xie
# Description:
#   (1) pull from local to master
#   (2) push from master to slave
# History:
#   (1) create
# Crontab -e
# */1 * * * * sh /home/ap/airflow/sbin/sync_slave.sh > /dev/null
##############################################################

sh /home/ap/airflow/sbin/pull_local.sh
sh /home/ap/airflow/sbin/push_slave.sh

exit 0