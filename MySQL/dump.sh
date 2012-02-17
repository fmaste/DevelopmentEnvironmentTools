#!/bin/bash

HOST=$1
DATABASE=$2
USER=$3
PASS=$4
TABLES=$5

# mysqldump [options] database [tables]
# OPTIONS
# --user=name: User for login if not current user.
# --password=name: Password to use when connecting to server.
# --host=name: Connect to host.
# --no-defaults: Don't read default options from any option file.
# --no-tablespaces: Do not dump any tablespace information.
# --add-drop-database: Add a DROP DATABASE before each create.
# --add-drop-table: Add a DROP TABLE before each create.
# --allow-keywords: Allow creation of column names that are keywords.
# --comments: Write additional information.
# --complete-insert: Use complete INSERT statements that include column names.
# --compress: Use compression in server/client protocol.
# --create-options: Include all MySQL specific create options.
# --disable-keys: surround the INSERT statements with statements to disable and enable keys.
# --extended-insert: Use multiple-row INSERT syntax that include several VALUES lists.
# --force: Continue even if we get an SQL error.
# --ignore-table=name: Do not dump the specified table.
# --no-data: No row information.
# --quick: Don't buffer query, dump directly to stdout.
# --quote-names: Quote table and column names with backticks (`).
# --replace: Use REPLACE INTO instead of INSERT INTO.
# --result-file=name: Direct output to a given file.
# --routines: Dump stored routines (functions and procedures).
# --set-charset: Add 'SET NAMES default_character_set' to the output.
# --triggers: Dump triggers for each dumped table.
# --tz-utc: SET TIME_ZONE='+00:00' at top of dump to allow dumping of TIMESTAMP data when a server has data in different time zones or data is being moved between servers with different time zones.
mysqldump --user=$USER --password=$PASS --host=$HOST --no-tablespaces --add-drop-database --add-drop-table --allow-keywords --comments --complete-insert --compress --create-options --disable-keys --extended-insert --no-data --quick --quote-names --routines --set-charset --triggers --tz-utc $DATABASE $TABLES

