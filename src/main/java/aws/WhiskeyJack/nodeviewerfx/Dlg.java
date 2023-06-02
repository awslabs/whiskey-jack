/*
 * SPDX-FileCopyrightText:  Amazon.com, Inc. or its affiliates. All Rights Reserved.
 * SPDX-License-Identifier: Apache-2.0
 */
package aws.WhiskeyJack.nodeviewerfx;

import static aws.WhiskeyJack.util.Utils.*;
import java.lang.reflect.*;
import java.util.*;
import javafx.scene.*;
import javafx.scene.control.*;
import javafx.scene.image.*;
import javafx.scene.layout.*;

public class Dlg {
    private static final int maxStringWidth = 60;
    private String title;
    private final VBox body = new VBox();
    private VBox expandable;
    public static void error(Object... o) {
        new Dlg().show(true, o);
    }
    public static void note(Object... o) {
        new Dlg().show(false, o);
    }
    private Dlg() {
    }
    private void show(boolean isError, Object... o) {
        var d = new Dialog();
        d.initOwner(GraphView.rootWindow());
        body.getStyleClass().clear();
        if(isError) {
            d.setGraphic(img(errorIcon));
            body.getStyleClass().add("errorDialog");
        } else {
            d.setGraphic(img(noteIcon));
            body.getStyleClass().add("noteDialog");
        }
        var dp = d.getDialogPane();
        dp.getStyleClass().add(isError ? "error" : "note");
        dp.setContent(body);
        addStuff(o, "dlgTitle");
        if(body.getChildren().isEmpty() && !isError) return;
        dp.setExpandableContent(expandable);
        d.setTitle(title == null ? "Error" : title);
        dp.getButtonTypes().add(ButtonType.OK);
        dp.getStylesheets().add(Dlg.class.getResource("error.css").toExternalForm());
        dp.setMinWidth(Region.USE_COMPUTED_SIZE);
        dp.setPrefWidth(Region.USE_COMPUTED_SIZE);
        dp.setMaxWidth(1000);
        d.setResizable(true);
        d.showAndWait();
    }
    private ImageView img(Image i) {
        var img = new ImageView(i);
            img.setPreserveRatio(true);
            img.setFitHeight(64);
            return img;
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
                    t.printStackTrace(System.out); // TODO eliminate
                    System.out.flush();
                    var u = getUltimateCause(t);
                    var m = u.getMessage();
                    if(isEmpty(m))
                        m = u.toString();
                    addStuff(m, style);
                    style = "dlgerrTitle";
                    if(expandable == null)
                        expandable = new VBox();
                    for(var e: u.getStackTrace())
                        addString(expandable, e.getMethodName() + '(' + e.getFileName() + ':' + e.getLineNumber() + ')', "dlgerrBody");
                }
                case Collection c ->
                    c.forEach(e -> addStuff(e, "dlgBody"));
                default -> {
                    addString(body, deepToString(o, maxStringWidth).toString(), style);
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
            else if(v.size() < 80) {
                var l = new Label(s);
                l.setWrapText(true);
                l.setPrefWidth(maxStringWidth*10);
                l.getStyleClass().add(style);
                v.add(l);
            }
    }
    static final private Image errorIcon = new Image(NodeView.class.getResourceAsStream("Oops.png"));
    static final private Image noteIcon = new Image(NodeView.class.getResourceAsStream("OK.png"));

}
