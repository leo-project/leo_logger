#!/bin/sh

make doc
rm -rf doc/rst && mkdir doc/rst

for Mod in leo_logger_appender_file \
           leo_logger_client_base \
           leo_logger_client_message \
           leo_logger_client_message \
           leo_logger_rotator \
           leo_logger_server \
           leo_logger_util
do
    read_file="doc/$Mod.html"
    write_file="doc/rst/$Mod.rst"

    pandoc --read=html --write=rst "$read_file" -o "$write_file"

    sed -ie "1,6d" "$write_file"
    sed -ie "1s/\Module //" "$write_file"
    LINE_1=`cat $write_file | wc -l`
    LINE_2=`expr $LINE_1 - 10`
    sed -ie "$LINE_2,\$d" "$write_file"
done
rm -rf doc/rst/*.rste
