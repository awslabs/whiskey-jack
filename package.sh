#! /bin/sh
# SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
# SPDX-License-Identifier: Apache-2.0
export JAVA_HOME=`/usr/libexec/java_home -v 17`
# mvn clean
mkdir target
mkdir target/mods
mvn package jpackage:jpackage@mac
