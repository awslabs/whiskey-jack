/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
module com.aws.jag.fxnodeeditor {
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

    opens aws.jag.DiagramEditor to javafx.fxml;
//    opens com.aws.jag.fxnodeeditor.graph to javafx.fxml;
    opens aws.jag.DiagramEditor.metadataeditor to javafx.fxml;
    opens aws.jag.DiagramEditor.nodeviewerfx to javafx.graphics, javafx.fxml;
    exports aws.jag.DiagramEditor;
}
