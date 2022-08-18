module com.aws.jag.fxnodeeditor {
    requires javafx.controls;
    requires javafx.fxml;
    requires javafx.base;
    requires javafx.graphics;
    requires java.base;
    requires java.prefs;
    requires jsr305;
    requires com.fasterxml.jackson.core;
    requires com.fasterxml.jackson.annotation;
    requires com.fasterxml.jackson.databind;
    requires com.fasterxml.jackson.dataformat.yaml;

    opens com.aws.jag.fxnodeeditor to javafx.fxml;
    opens com.aws.jag.fxnodeeditor.graph to javafx.fxml;
    opens com.aws.jag.fxnodeeditor.metaedit to javafx.fxml;
    exports com.aws.jag.fxnodeeditor;
}
