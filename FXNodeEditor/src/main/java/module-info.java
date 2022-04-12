module com.nighthacks.fxnodeeditor {
    requires javafx.controls;
    requires javafx.fxml;
    requires javafx.base;
    requires javafx.graphics;
    requires javafx.web;
    requires java.base;
    requires jsr305;
    requires com.fasterxml.jackson.core;
    requires com.fasterxml.jackson.annotation;
    requires com.fasterxml.jackson.databind;
    requires com.fasterxml.jackson.dataformat.yaml;

    opens com.nighthacks.fxnodeeditor to javafx.fxml;
    exports com.nighthacks.fxnodeeditor;
}
