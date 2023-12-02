/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
module aws.WhiskeyJack {
    requires javafx.controls;
    requires javafx.fxml;
    requires javafx.base;
    requires javafx.graphics;
    requires java.base;
    requires java.prefs;
    requires io.github.classgraph;
    requires jsr305;
    requires com.fasterxml.jackson.core;
    requires com.fasterxml.jackson.annotation;
    requires com.fasterxml.jackson.databind;
    requires com.fasterxml.jackson.dataformat.yaml;
    opens aws.WhiskeyJack.metadataeditor to javafx.fxml;
    opens aws.WhiskeyJack.nodeviewerfx to javafx.graphics, javafx.fxml;
    exports aws.WhiskeyJack;
    requires javafx.graphicsEmpty;
}
