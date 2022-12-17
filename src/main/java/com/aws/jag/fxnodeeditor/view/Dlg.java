/*
 * Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package com.aws.jag.fxnodeeditor.view;

import static com.aws.jag.fxnodeeditor.util.Utils.*;
import java.lang.reflect.*;
import java.util.*;
import javafx.scene.*;
import javafx.scene.control.*;
import javafx.scene.image.*;
import javafx.scene.layout.*;

public class Dlg {
    private String title;
    private final VBox body = new VBox();
    private VBox expandable;
    public static void error(Object... o) {
        new Dlg().show(true, o);
    }
    private Dlg() {
    }
    private void show(boolean isError, Object... o) {
        var d = new Dialog();
        body.getStyleClass().clear();
        if(isError) {
            var img = new ImageView(errorIcon);
            img.setPreserveRatio(true);
            img.setFitHeight(64);
            d.setGraphic(img);
            body.getStyleClass().add("errorDialog");
        } else
            body.getStyleClass().add("noteDialog");
        var dp = d.getDialogPane();
        dp.getStyleClass().add(isError ? "error" : "note");
        dp.setContent(body);
        addStuff(o, "dlgTitle");
        dp.setExpandableContent(expandable);
        d.setTitle(title == null ? "Error" : title);
        dp.getButtonTypes().add(ButtonType.OK);
        dp.getStylesheets().add(Dlg.class.getResource("error.css").toExternalForm());
        d.showAndWait();
    }
    private String addStuff(Object o, String style) {
        if(o == null)
            return style;
        if(o.getClass().isArray()) {
            var len = Array.getLength(o);
            for(var i = 0; i < len; i++)
                style = addStuff(Array.get(o, i), style);
        } else
            switch(o) {
                case null -> {
                }
                case Node n ->
                    body.getChildren().add(n);
                case Throwable t -> {
                    System.out.flush();
                    var u = getUltimateCause(t);
                    var m = u.getMessage();
                    if(isEmpty(m))
                        m = u.toString();
                    addStuff(m, "dlgerrTitle");
                    if(expandable == null)
                        expandable = new VBox();
                    for(var e: u.getStackTrace())
                        addString(expandable, e.getMethodName() + '(' + e.getFileName() + ':' + e.getLineNumber() + ')', "dlgerrBody");
                }
                case Collection c ->
                    c.forEach(e -> addStuff(e, "dlgBody"));
                default -> {
                    addString(body, deepToString(o, 80).toString(), style);
                    style = "dlgBody";
                }
            }
        return style;
    }
    private void addString(VBox dest, String s, String style) {
        var v = dest.getChildren();
        if(!isEmpty(s))
            if(title == null)
                title = s;
            else if(v.size() < 20) {
                var l = new Label(s);
                l.setWrapText(true);
                l.getStyleClass().add(style);
                v.add(l);
            }
    }
    public static void note(Object... o) {
        new Dlg().show(false, o);
    }
    static final private Image errorIcon = new Image(NodeView.class.getResourceAsStream("Oops.png"));

}
