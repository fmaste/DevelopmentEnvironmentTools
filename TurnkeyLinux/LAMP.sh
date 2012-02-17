#!/bin/bash

# Download Turnkey Linux LAMP vmdk image file.

# The output file name (".vmdk" is appended).
# As temporary files $FILE.zip and $FILE.unzip are also created.
# This files are not deleted for security reasons.
FILE=$1

# Where to download the file from.
DOWNLOAD_PATH="http://downloads.sourceforge.net/project/turnkeylinux/vmdk/"
# The file name on the server.
DOWNLOAD_FILE_NAME="turnkey-lamp-11.3-lucid-x86-vmdk.zip"
# Sourceforge send a HTTP 302 code.
curl --verbose --location --output $FILE.zip $DOWNLOAD_PATH$DOWNLOAD_FILE_NAME

# Unzip the file with an output file.
mkdir $FILE.unzip
unzip $FILE.zip -d $FILE.unzip

# The unzipped folder.
UNZIP_FOLDER="turnkey-lamp-11.3-lucid-x86"
# The .vmdk file name inside the unzipped folder.
UNZIP_VMDK_FILE_NAME="turnkey-lamp-11.3-lucid-x86.vmdk"

# Move out the .vmdk file.
cp $FILE.unzip/$UNZIP_FOLDER/$UNZIP_VMDK_FILE_NAME $FILE.vmdk

